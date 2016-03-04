var soundPlayer = new Howl(soundSprite);

var elm = Elm.fullscreen(Elm.Main);

elm.ports.sounds.subscribe(function(sound) {
  soundPlayer.play(sound);
});
