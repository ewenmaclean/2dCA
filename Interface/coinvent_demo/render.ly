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

 <ees aes >2   <ees e g aes >2   <c ees aes >2  
}

lower = \relative c {
  \clef bass
  \key c \major
  \set Staff.midiInstrument = #"piano"
%  \time 4/4

 e2  c2  f2 
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
