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