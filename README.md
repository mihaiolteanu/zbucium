# zbucium
[last.fm](https://www.last.fm/home) music player with lyrics and scrobble support, among other things.

This is a Common Lisp library that can be used to build a more interactive music
player on top, like the
[zbucium-stump](https://github.com/mihaiolteanu/zbucium-stump) player for the
StumpWM window manager for which this library was created in the first
place. But it can also be used as such. It depends on the
[lastfm](https://github.com/mihaiolteanu/lastfm),
[youtube](https://github.com/mihaiolteanu/youtube) and
[lyrics](https://github.com/mihaiolteanu/lyrics) libraries by the same
author. The songs are played from youtube with the mpv player, in the background,
with input from last.fm.

# Installation
Install [lastfm](https://github.com/mihaiolteanu/lastfm),
[youtube](https://github.com/mihaiolteanu/youtube) and the
[lyrics](https://github.com/mihaiolteanu/lyrics) Common Lisp libraries from the
same author. These are not yet in the quicklisp database but were developed
specifically for this project. Since they might be useful in other context, they
were written as libraries. 

```bash
# clone to local-projects for quickload access
git clone https://github.com/mihaiolteanu/zbucium ~/quicklisp/local-projects/zbucium
```

```common-lisp
; Register the new project
(ql:register-local-projects)
```

# Usage

You tell `zbucium` what to play and it will open an audio-only mpv instance in
the background and play it. You can play single songs, single albums and, more
interestingly, discover new music by playing similar artists or top songs from a
given genre. You chose how many artists and songs to take into
consideration. Fewer, for topsongs and hits, many for a more eclectic experience.

```common-lisp
(play-song "anathema" "one last goodbye")
```

The lyrics of the current playing song can also be retrieved.
```common-lisp
(song-lyrics)
    => "How I needed you
        How I grieve now you're gone
        In my dreams I see you ...."
```

Even if you don't request the lyrics, they are requested and saved by
default.

All the other functionality you would expect from a player are now available,
like play/pause, seek, stop or request the opening of the youtube page in your
default browser.

But you can also feed the player an infinite stream of songs, thanks to the
[generators](https://github.com/mihaiolteanu/lastfm#generators) provided by the
lastfm library. This will play the best 30 songs from `anathema`
(based on the last.fm recomendations), at random, until yout `stop` it or until
you chose to play something else.
```common-lisp
(play-artist "anathema" 30 T)
```

Or play random songs from random artists similar to the given artist. You
have to select how many artists to consider and how many songs for each of those
artists to take into consideration. The lower the value, the better the chances
to play more similar artists and more top songs/hits for thos similar
artists. The higher the values, the higher the chances to discover hidden gems.
```common-lisp
; Play the best three matching artists and play from the best ten songs for
; those matching artists
(play-artist-similar "anathema" 3 10)
```

```common-lisp
(what-is-playing)
    => ("Riverside" "Second Life Syndrome")
    
(next-song)
(what-is-playing)
    => ("Katatonia" "Leaders")
    
(stop)
```

# API

**play-song** _artist song_

    Play a single song and then replay it after it finishes.

**play-artist** _artist nsongs random_

    Play the best nsongs, as reported by last.fm, for the given artist, in
    random order or one after another. 

**play-album** _artist album_

    Play a single album from the given artist.

**play-tag** _tag nsongs random_

    Play the best songs for this tag (genre, usually). There is a similar
    `play-tag-similar` function but the results are different.

**play-user-songs** _username nsongs random_

    Play the first nsongs from the list of user loved songs. Can be played at
    random or one after another.

**play-my-loved-songs** _nsongs random_

    Play from the list of my loved songs. The username in this case is base on
    the lastfm configuration.

**play-artist-similar** _artist nartists nsongs_

    Picks a random artist from the top nartists that last.fm says are the most
    similar to this artist. After that, pick a random song from the first best
    nsongs of the selected artist and play it. 

**play-tag-similar** _tag nartists nsongs_

    Picks a random artist from the top nartists that last.fm says are the most
    representative for this tag (genre, usually). After that, pick a random song
    from the first best nsongs of the selected artist and play it.

**what-is-playing**

    Return an array with two strings. The first string is the currently playing
    artist and the seconds one the currently playing song name. Both are NIL if
    nothing is playing.
    
**what-is-playing-as-string**

    Return a printable representation of the artist and song currently
    playing. Returns NIL if the player is stopped.

**song-lyrics**

    Get the song lyrics for the currently playing song

**love-song**

    Use the lastfm library to add the currently playing song to the last.fm
    loved list for the configured user. Authentication is needed for this
    function to work. See the `lastfm` installation requirements.

**unlove-song**

    Similar as `love-song`, only it removes the songs from the list.

**next-song**
    
    Play the next songs in the playlist

**stop**

    Stop the player


## Interfaces exported from `lyrics`

**search-song**

    Return a list of entries, where each entry is a list with the artist name,
    the song name and the verse line where the lyrics appears. The artist name and
    song can be used to request the full lyrics. There can be multiple entries for
    the same artist/song combination.


## Interfaces Exported from `youtube`

**play/pause**
  
    Toggle playing status.

**replay**
  
    Rewind the song at the beginning, effectively replaying it.
  
**seek** _seconds_
  
    Forward or backward play by seconds, if the seconds is negative.
  
**percent-pos**
  
    Current playing song position, in percent.
  
**time-pos**
  
    Current playing song position, in seconds, as string.
  
**duration**
  
    Current playing song duration, in MM:SS format, as string.
  
**switch-to-browser** _&key (from-beginning nil)_
    
    Pause the player and open the youtube page of the current playing song in the
    user default browser. If `from-beginning` is T, start playing from the beginning,
    otherwise continue from where the player was. 

**turn-video-on**
  
    Quit mpv and restart it in video mode, locally (i.e. not in the browser)


## Authors
Copyright (c) 2019 [Mihai Olteanu](www.mihaiolteanu.me)

Licensed under the [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html) license.

