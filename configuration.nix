# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:


  let
    nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
      export __NV_PRIME_RENDER_OFFLOAD=1
      export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
      export __GLX_VENDOR_LIBRARY_NAME=nvidia
      export __VK_LAYER_NV_optimus=NVIDIA_only
      exec -a "$0" "$@"
    '';
  in
  {
  
  # NVIDIA STUFF
  nixpkgs.config.allowUnfree = true;
  services.xserver.videoDrivers = [ "nvidia" "modeset" ];
  hardware.nvidia = {
    prime = {
	    offload.enable = true;

	    # Bus ID of the Intel GPU. You can find it using lspci, either under 3D or VGA
	    intelBusId = "PCI:0:2:0";

	    # Bus ID of the NVIDIA GPU. You can find it using lspci, either under 3D or VGA
	    nvidiaBusId = "PCI:1:0:0";
    };
  };

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];


  # Use the systemd-boot EFI boot loader.
  # boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.enable = true;
  boot.loader.grub.devices = [ "nodev" ];
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.useOSProber = true;

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp3s0.useDHCP = true;
  networking.interfaces.wlo1.useDHCP = true;
  networking.networkmanager.enable = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  # Enable SDDM
#  services.xserver.displayManager.sddm.enable = true;
  # Enable GDM
  services.xserver.displayManager.gdm.enable = true;

  # Gnome or Plasma?
  services.xserver.desktopManager.gnome.enable = true;
# services.xserver.desktopManager.plasma5.enable = true;

 
  # use NVIDIA card
  # services.xserver.videoDrivers = [ "intel" ];
  # hardware.bumblebee.enable = true;
  # hardware.opengl.driSupport32Bit = true;
  
  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.tomazgomes = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ]; 
  };



  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    neovim
    emacs
    vscode

    git
    curl
    wget

    zsh
    tmux
    alacritty
    ranger
    htop
    neofetch
    fzf
    ripgrep

    zoom-us
    slack
    tdesktop
    discord
    
    google-chrome

    gcc
    glib
    gnumake
    python
    ghc
    cargo
    agda
    lean
    nodejs

    ccls
    haskellPackages.haskell-language-server
    nodePackages.typescript

    networkmanager
    lxappearance
    gnome3.gnome-tweak-tool
    pop-gtk-theme
    pop-icon-theme
    home-manager

    nvidia-offload 
  ];

  fonts.fonts = with pkgs; [
    fira-code
    fira-mono
    fira
    (nerdfonts.override { fonts = [ "FiraCode" "FiraMono" ]; })
    roboto-slab
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
}

