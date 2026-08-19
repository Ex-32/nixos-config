{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: let
  ZMQ_PORT = "42424";
  WALLET = "45NZ1VGe7V9VtJjSZcji8aMYUrsKvtERRPSGphTSh9W5UnBJXT7TrWaV7k1wMFs3oGBASGChLoKLt1CZx1vTx4GHTzUxgr5";
  POOL_ADDR = "10.67.69.1";
in {
  allowedUnfree = [
    "cuda-merged"
    "cuda_cuobjdump"
    "cuda_gdb"
    "cuda_nvcc"
    "cuda_nvdisasm"
    "cuda_nvprune"
    "cuda_cccl"
    "cuda_cudart"
    "cuda_cupti"
    "cuda_cuxxfilt"
    "cuda_nvml_dev"
    "cuda_nvrtc"
    "cuda_nvtx"
    "cuda_profiler_api"
    "cuda_sanitizer_api"
    "libcublas"
    "libcufft"
    "libcurand"
    "libcusolver"
    "libnvjitlink"
    "libcusparse"
    "libnpp"
  ];

  boot.kernelParams = [
    "hugepagesz=1G"
    "hugepages=16"
    "amd_pstate=active"
  ];

  powerManagement = {
    cpuFreqGovernor = "performance";
  };

  # networking.interfaces.enp9s0.ipv4.addresses = [
  #   {
  #     address = POOL_ADDR;
  #     prefixLength = 24;
  #   }
  # ];

  services.monero = {
    enable = true;
    prune = true;
    dataDir = "/mnt/xmr";
    extraConfig = ''
      zmq-rpc-bind-ip=127.0.0.1
      zmq-rpc-bind-port=18083
      zmq-pub=tcp://127.0.0.1:${ZMQ_PORT}
    '';
  };

  services.xmrig = {
    enable = true;
    package = pkgs.xmrig;
    settings = {
      autosave = true;
      cpu = {
        enabled = true;
        yield = true;
      };
      randomx = {
        mode = "fast";
        "1gb-pages" = true;
      };
      cuda = {
        enabled = true;
        loader = "${pkgs.xmrig-cuda}/lib/libxmrig-cuda.so";
      };
      pools = [
        {
          url = "127.0.0.1:3333";
          user = WALLET;
          keepalive = true;
          tls = true;
        }
      ];
    };
  };

  systemd.services."p2pool" = {
    description = "Distributed XMR pool";
    wantedBy = ["multi-user.target"];
    after = ["network.target"];
    before = ["xmrig.service"];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${lib.getExe pkgs.p2pool} --mini --host 127.0.0.1 --rpc-port 18081 --wallet ${WALLET} --zmq-port ${ZMQ_PORT} --stratum 127.0.0.1:3333,${POOL_ADDR}:3333";
      Restart = "on-failure";
      RestartSec = 1;
      RestartStopSec = 10;
    };
  };

  environment.systemPackages = [
    config.services.xmrig.package
    pkgs.monero-cli
    pkgs.p2pool
  ];
}
