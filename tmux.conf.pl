#!/usr/bin/env perl

use strict;
use warnings;

my $tmux_command = "tmux";
my @tmux_set_command = ("set", "-g");
my @tmux_setw_command  = ("setw", "-g");

my %color_prefs = (
    "arch-aspireone" => "blue,white",
    "darwin-mba.local" => "yellow,black",
    "newkiwi" => "magenta,white"
    );
my $color_def = "green,white";

sub tmux {
    my @command = ($tmux_command, );
    push(@command, @_);
    # print "@command, \n";
    system(@command) == 0
        or die "system @command failed: $?";
}

sub set {
    my @command = @tmux_set_command;
    push(@command, @_);
    tmux(@command);
}

sub setw {
    my @command = @tmux_setw_command;
    push(@command, @_);
    tmux(@command);
}

sub set_key {
    tmux("unbind", "C-b");
    set("prefix", "C-z");
    tmux("bind-key", "C-z", "send-prefix");
    tmux("bind-key", "c", "command-prompt", "new-window '%%'");
}

sub set_prefs {
    # this does not workd because `run' do script asyncly
    set("base-index", "1");
    set("pane-base-index", "1");
    set("renumber-windows", "on");
    setw("mode-keys", "vi");
    set("default-command", "/bin/bash");
    set("default-path", $ENV{"HOME"});
    set("set-titles", "on");
    set("display-panes-time", "5000");
}

sub get_hostname {
    my $hostname = $ENV{"HOSTNAME"};
    if (! $hostname) {
        $hostname = `hostname`;
        $hostname =~ s/\n//;
    }
    return $hostname;
}

sub set_status_line {
    my $user = $ENV{"USER"};
    my $hostname = get_hostname();
    my $tmux_v = `tmux -V`;
    $tmux_v =~ s/\n//;
    set("status-right", "${user}\@${hostname} | ${tmux_v} ");
}

sub set_colors {
    my $hostname = get_hostname();
    my $color = $color_prefs{$hostname};
    if (! $color) {
        $color = $color_def;
    }
    my ($bg, $fg) = split(/,/, $color);
    set("status-bg", $bg);
    set("status-fg", $fg);
    set("mode-bg", $bg);
    set("mode-fg", $fg);
    set("pane-active-border-fg", $bg);

    set("message-bg", "white");
    set("message-fg", "black");

    setw("window-status-current-bg", "white");
    setw("window-status-current-fg", "black");
    setw("window-status-current-attr", "bold");
}

sub main {
    set_key();
    set_prefs();
    set_status_line();
    set_colors();
}

main();
