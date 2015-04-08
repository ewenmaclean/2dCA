\version "2.16.2"

%% default is some a4-like format
% \paper {
%    #(define paper-height (* 4 cm))
%    #(define paper-width (* 5 cm))
%    #(define line-width (* 4 cm))
%    #(define top-margin (* 0 cm))
%    #(define left-margin (* 0.5 cm))
%    #(define indent (* 0 cm))
% }

%% avoid LilyPond advert getting in the way

\header {
  tagline = ""  % removed
}

upper = \relative c' {
  \clef treble
  \key c \major
  \set Staff.midiInstrument = #"piano"
%  \time 4/4

 <dis fis ais >4  <fis a c>  <b, d fis> <d f aes>  
<g bes d> <ais, cis e> <dis fis ais>2
}

lower = \relative c {
  \clef bass
  \key c \major
  \set Staff.midiInstrument = #"piano"
%  \time 4/4

<b>4 <d>4 <g>4 <bes,>4 
<ees>4 <fis>4 <b>2
}

\score {
  \new PianoStaff <<
%%  \set PianoStaff.instrumentName = #"Piano  "
    \new Staff = "upper" \upper
    \new Staff = "lower" \lower
  >>
  \layout {
%% no time signature
    \context {
      \Staff \remove Time_signature_engraver
    }  }
  \midi { }
}
