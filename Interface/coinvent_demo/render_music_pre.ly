\version "2.16.2"

%% default is some a4-like format
\paper {
   #(define paper-height (* 4 cm))
   #(define paper-width (* 5 cm))
   #(define line-width (* 4 cm))
   #(define top-margin (* 0 cm))
   #(define left-margin (* 0.5 cm))
   #(define indent (* 0 cm))
}

%% avoid LilyPond advert getting in the way

\header {
  tagline = ""  % removed
}

upper = \relative c' {
  \clef treble
  \key c \major
  \set Staff.midiInstrument = #"piano"
%  \time 4/4
