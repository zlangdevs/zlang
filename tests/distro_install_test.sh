#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ZIG_VERSION="0.16.0"
LLVM_VERSION="22"

PARALLEL=6
MEM="512m"
CPUS="1"
FILTER_DISTRO=""
FILTER_ARCH=""
DO_CLEAN=0

R='\033[0;31m' G='\033[0;32m' Y='\033[0;33m' B='\033[1m' N='\033[0m'

IMAGE_PREFIX="zlang-deps"
RESULT_DIR=""

declare -A IMAGES=(
    [ubuntu-24.04]="ubuntu:24.04"
    [debian-trixie]="debian:trixie-slim"
    [fedora-minimal-40]="registry.fedoraproject.org/fedora-minimal:40"
    [arch]="archlinux:base"
    [alpine-edge]="alpine:edge"
    [opensuse-tw]="opensuse/tumbleweed"
    [void]="ghcr.io/void-linux/void-linux:latest-thin-x86_64"
)

ARCHS=("linux/amd64" "linux/arm64" "linux/riscv64")

ARCH_SKIP=(
    "arch:linux/arm64"
    "arch:linux/riscv64"
    "void:linux/arm64"
    "void:linux/riscv64"
    "fedora-minimal-40:linux/riscv64"
    "ubuntu-24.04:linux/riscv64"
    "debian-trixie:linux/riscv64"
)

setup_ubuntu_debian() {
    cat <<SETUP
export DEBIAN_FRONTEND=noninteractive
apt-get update -qq
apt-get install -y --no-install-recommends ca-certificates wget gnupg lsb-release flex bison make xz-utils
codename=\$(lsb_release -cs 2>/dev/null || echo bookworm)
wget -qO- https://apt.llvm.org/llvm-snapshot.gpg.key | gpg --dearmor > /etc/apt/trusted.gpg.d/llvm-snapshot.gpg
printf 'deb http://apt.llvm.org/%s/ llvm-toolchain-%s-${LLVM_VERSION} main\n' "\$codename" "\$codename" > /etc/apt/sources.list.d/llvm.list
apt-get update -qq
apt-get install -y --no-install-recommends llvm-${LLVM_VERSION} lld-${LLVM_VERSION} llvm-${LLVM_VERSION}-dev
for t in llc opt lld ld.lld llvm-config llvm-link llvm-ar; do
    [ -f /usr/bin/\${t}-${LLVM_VERSION} ] && \
        ln -sf /usr/bin/\${t}-${LLVM_VERSION} /usr/bin/\$t || true
done
so=\$(find /usr/lib -name "libLLVM-${LLVM_VERSION}.so*" -o -name "libLLVM.so.${LLVM_VERSION}*" 2>/dev/null | sort | tail -1)
[ -n "\$so" ] && ln -sf "\$so" /usr/lib/libLLVM-${LLVM_VERSION}.so || true
SETUP
}

setup_fedora() {
    cat <<'SETUP'
microdnf install -y llvm lld llvm-devel flex bison make wget findutils xz tar
SETUP
}

setup_arch() {
    cat <<'SETUP'
pacman -Syu --noconfirm
pacman -S --noconfirm --needed llvm lld flex bison make wget xz ca-certificates
SETUP
}

setup_alpine() {
    cat <<'SETUP'
apk update
apk add --no-cache llvm-dev lld flex bison make wget xz musl-dev bash
for cfg in /usr/lib/llvm*/bin/llvm-config; do
    [ -f "$cfg" ] && ln -sf "$cfg" /usr/local/bin/llvm-config && break || true
done
if [ ! -f /usr/include/llvm-c/Core.h ]; then
    core_h=$(find /usr/lib/llvm* -name 'Core.h' -path '*/llvm-c/*' 2>/dev/null | head -1)
    if [ -n "$core_h" ]; then
        rm -rf /usr/include/llvm-c 2>/dev/null
        ln -sf "$(dirname "$core_h")" /usr/include/llvm-c
    fi
fi
so=$(find /usr/lib/llvm*/lib -name 'libLLVM*.so*' 2>/dev/null | sort | tail -1)
[ -n "$so" ] && ln -sf "$so" /usr/lib/"$(basename "$so")" 2>/dev/null || true
for lld_bin in /usr/lib/llvm*/bin/ld.lld /usr/bin/ld.lld; do
    [ -f "$lld_bin" ] && ln -sf "$lld_bin" /usr/local/bin/ld.lld && break || true
done
SETUP
}

setup_opensuse() {
    cat <<'SETUP'
zypper --non-interactive refresh
zypper --non-interactive install -y --no-recommends llvm lld llvm-devel flex bison make wget xz tar
SETUP
}

setup_void() {
    cat <<'SETUP'
xbps-install -Syu xbps
xbps-install -Syu
xbps-install -y llvm22 llvm22-devel libllvm22 flex bison make wget xz tar bash
for cfg in /usr/lib/llvm/22/bin/llvm-config /usr/bin/llvm-config-22 /usr/bin/llvm-config; do
    [ -x "$cfg" ] && ln -sf "$cfg" /usr/local/bin/llvm-config && ln -sf "$cfg" /usr/bin/llvm-config && break || true
done
for lld_bin in /usr/lib/llvm/22/bin/ld.lld /usr/bin/ld.lld-22 /usr/bin/ld.lld; do
    [ -x "$lld_bin" ] && ln -sf "$lld_bin" /usr/local/bin/ld.lld && break || true
done
for ver_lib in /usr/lib/llvm/*/lib; do
    so=$(ls "${ver_lib}"/libLLVM*.so 2>/dev/null | sort | tail -1)
    [ -n "$so" ] && ln -sf "$so" /usr/lib/"$(basename "$so")" 2>/dev/null && break || true
done
SETUP
}

distro_zig_build_args() {
    local distro="$1" arch="$2"
    local zig_arch
    case "$arch" in
        linux/amd64)   zig_arch="x86_64";;
        linux/arm64)   zig_arch="aarch64";;
        linux/riscv64) zig_arch="riscv64";;
        *)             zig_arch="";;
    esac
    case "$distro" in
        alpine-*)
            [ -n "$zig_arch" ] && echo "-Dtarget=${zig_arch}-linux-musl" || echo "";;
        *) echo "";;
    esac
}

distro_setup() {
    case "$1" in
        ubuntu-*|debian-*)  setup_ubuntu_debian;;
        fedora-*)            setup_fedora;;
        arch)                setup_arch;;
        alpine-*)            setup_alpine;;
        opensuse-*)          setup_opensuse;;
        void)                setup_void;;
        *) printf 'echo "no setup for %s"; exit 1\n' "$1";;
    esac
}

zig_install() {
    cat <<SETUP
ZIG_ARCH=\$(uname -m)
case "\$ZIG_ARCH" in
    x86_64)  ZA="x86_64";;
    aarch64) ZA="aarch64";;
    riscv64) ZA="riscv64";;
    *) echo "unsupported arch: \$ZIG_ARCH" >&2; exit 1;;
esac
ZIG_TAR="zig-\${ZA}-linux-${ZIG_VERSION}.tar.xz"
ZIG_URL="https://ziglang.org/download/${ZIG_VERSION}/\${ZIG_TAR}"
wget -nv -O /tmp/\${ZIG_TAR} "\$ZIG_URL"
tar -xf /tmp/\${ZIG_TAR} -C /usr/local/
ln -sf /usr/local/zig-\${ZA}-linux-${ZIG_VERSION}/zig /usr/local/bin/zig
zig version
SETUP
}

build_test_image() {
    local distro="$1" arch="$2"
    local tag="${IMAGE_PREFIX}-${distro}-${arch//\//-}"
    local ctx
    ctx=$(mktemp -d)
    mkdir "$ctx/src"

    distro_setup "$distro" > "$ctx/setup.sh"
    zig_install > "$ctx/zig_install.sh"
    rsync -a --exclude='.git' --exclude='.zig-cache' --exclude='zig-out' \
        "${REPO_ROOT}/" "$ctx/src/"

    local extra_build_args
    extra_build_args=$(distro_zig_build_args "$distro" "$arch")

    cat > "$ctx/Dockerfile" <<DOCKERFILE
FROM --platform=${arch} ${IMAGES[$distro]} AS deps
ARG ZLANG_TEST_ARCH=${arch}
COPY setup.sh /tmp/setup.sh
RUN sh /tmp/setup.sh
COPY zig_install.sh /tmp/zig_install.sh
RUN sh /tmp/zig_install.sh

FROM deps AS built
COPY src/ /zlang/
WORKDIR /zlang
RUN zig build -Doptimize=ReleaseFast ${extra_build_args} -j1
RUN ./zig-out/bin/zlang -version
DOCKERFILE

    docker pull --platform "$arch" "${IMAGES[$distro]}" 2>&1
    docker build --platform "$arch" --no-cache -t "$tag" --label "zlang-test=1" "$ctx" 2>&1
    local rc=$?
    rm -rf "$ctx"
    return $rc
}

run_test_in_image() {
    local distro="$1" arch="$2"
    local tag="${IMAGE_PREFIX}-${distro}-${arch//\//-}"

    docker run --rm \
        --platform "$arch" \
        --memory="$MEM" \
        --cpus="$CPUS" \
        -e ZLANG_TEST_JOBS=1 \
        -e USE_APPIMAGE=0 \
        -e CLEAR_EACH_TEST=0 \
        -e CLEAR_BEFORE_SUMMARY=0 \
        "$tag" \
        sh -c '
            cat > /tmp/zig <<'"'"'ZIGWRAP'"'"'
#!/bin/sh
if [ "$1" = "build" ] && [ -z "${2:-}" ] && [ -x /zlang/zig-out/bin/zlang ]; then
    exit 0
fi
exec /usr/local/bin/zig "$@"
ZIGWRAP
            chmod +x /tmp/zig
            export PATH="/tmp:$PATH"
            cd /zlang && ./run_tests.sh
        ' 2>&1
}

is_arch_skipped() {
    local distro="$1" arch="$2"
    local entry
    for entry in "${ARCH_SKIP[@]}"; do
        [[ "$entry" == "${distro}:${arch}" ]] && return 0
    done
    return 1
}

write_result() {
    local status="$1" distro="$2" arch="$3" detail="${4:-}"
    local key="${distro}_${arch//\//-}"
    printf '%s\n' "$status" > "${RESULT_DIR}/${key}.status"
    [[ -n "$detail" ]] && printf '%s' "$detail" > "${RESULT_DIR}/${key}.log" || true
}

run_one() {
    local distro="$1" arch="$2"
    local label="${distro} @ ${arch##*/}"

    if is_arch_skipped "$distro" "$arch"; then
        printf "${Y}SKIP${N}  %s\n" "$label"
        write_result SKIP "$distro" "$arch"
        return
    fi

    printf "${B}....${N}  %s\n" "$label"

    local out
    if ! out=$(build_test_image "$distro" "$arch" 2>&1); then
        if printf '%s' "$out" | grep -qi "manifest unknown\|no such image\|platform.*not.*supported\|image.*not.*found\|pull access denied"; then
            printf "${Y}SKIP${N}  %s  (image/platform unavailable)\n" "$label"
            write_result SKIP "$distro" "$arch"
        else
            printf "${R}FAIL${N}  %s  (deps build failed)\n" "$label"
            printf '%s\n' "$out" | tail -30 | sed 's/^/       /'
            write_result FAIL "$distro" "$arch" "$out"
        fi
        return
    fi

    if out=$(run_test_in_image "$distro" "$arch" 2>&1); then
        printf "${G}PASS${N}  %s\n" "$label"
        write_result PASS "$distro" "$arch"
    else
        printf "${R}FAIL${N}  %s\n" "$label"
        printf '%s\n' "$out" | tail -40 | sed 's/^/       /'
        write_result FAIL "$distro" "$arch" "$out"
    fi
}

run_all() {
    RESULT_DIR=$(mktemp -d)

    local fifo
    fifo=$(mktemp -u)
    mkfifo "$fifo"
    exec 9<>"$fifo"
    rm "$fifo"
    for (( i=0; i<PARALLEL; i++ )); do printf 'x' >&9; done

    local pids=()
    for distro in $(printf '%s\n' "${!IMAGES[@]}" | sort); do
        [[ -n "$FILTER_DISTRO" && "$distro" != "$FILTER_DISTRO" ]] && continue
        for arch in "${ARCHS[@]}"; do
            [[ -n "$FILTER_ARCH" && "$arch" != "$FILTER_ARCH" ]] && continue
            read -rn1 -u9
            (
                run_one "$distro" "$arch"
                printf 'x' >&9
            ) &
            pids+=($!)
        done
    done

    for pid in "${pids[@]}"; do wait "$pid" || true; done
    exec 9>&-

    local pass=0 fail=0 skip=0
    for f in "$RESULT_DIR"/*.status; do
        [ -f "$f" ] || continue
        case "$(cat "$f")" in
            PASS) pass=$(( pass + 1 ));;
            FAIL) fail=$(( fail + 1 ));;
            SKIP) skip=$(( skip + 1 ));;
        esac
    done

    echo
    printf "${B}Results:${N} ${G}%d passed${N}  ${R}%d failed${N}  ${Y}%d skipped${N}\n" \
        "$pass" "$fail" "$skip"

    if [[ "$fail" -gt 0 ]]; then
        echo
        printf "${B}Failed:${N}\n"
        for f in "$RESULT_DIR"/*.status; do
            [[ "$(cat "$f")" == "FAIL" ]] || continue
            key="${f%.status}"
            key="${key##*/}"
            echo "  ${key//_/ @ }"
        done
    fi

    rm -rf "$RESULT_DIR"
    [[ "$fail" -eq 0 ]]
}

do_clean() {
    printf 'Removing zlang test images...\n'
    docker images --filter "label=zlang-test=1" -q | xargs -r docker rmi -f
    printf 'Done.\n'
}

print_usage() {
    cat <<USAGE
Usage: $0 [clean] [--distro NAME] [--arch ARCH] [--parallel N] [--mem SIZE] [--cpus N] [--list]

  clean            remove all zlang test images
  --distro NAME    run only this distro (see --list)
  --arch ARCH      run only this arch (e.g. linux/arm64)
  --parallel N     max concurrent containers (default: ${PARALLEL})
  --mem SIZE       memory limit per container (default: ${MEM})
  --cpus N         cpu limit per container (default: ${CPUS})
  --list           print distros and archs, exit
USAGE
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        clean)       DO_CLEAN=1; shift;;
        --distro)    FILTER_DISTRO="$2"; shift 2;;
        --arch)      FILTER_ARCH="$2"; shift 2;;
        --parallel)  PARALLEL="$2"; shift 2;;
        --mem)       MEM="$2"; shift 2;;
        --cpus)      CPUS="$2"; shift 2;;
        --list)
            printf 'Distros:\n'
            for d in $(printf '%s\n' "${!IMAGES[@]}" | sort); do
                printf '  %-25s %s\n' "$d" "${IMAGES[$d]}"
            done
            printf 'Archs:\n'
            for a in "${ARCHS[@]}"; do printf '  %s\n' "$a"; done
            exit 0;;
        -h|--help) print_usage; exit 0;;
        *) printf 'unknown: %s\n' "$1"; print_usage; exit 1;;
    esac
done

if [[ "$DO_CLEAN" -eq 1 ]]; then
    do_clean
    exit 0
fi

printf "${B}zlang install tests${N}  zig=%s llvm=%s  parallel=%s mem=%s cpus=%s\n\n" \
    "$ZIG_VERSION" "$LLVM_VERSION" "$PARALLEL" "$MEM" "$CPUS"

run_all
