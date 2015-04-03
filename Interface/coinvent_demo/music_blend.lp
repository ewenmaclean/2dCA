#program base.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% spec G7 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spec(spec_G7).
hasId(spec_G7,0).

%% sorts %%
hasSort(spec_G7,sort_Chord,0).
isNonDataSort(spec_G7,sort_Chord).
sortHasPriority(spec_G7,sort_Chord,1).

hasSort(spec_G7,sort_Note,0).
isDataSort(spec_G7,sort_Note).
sortHasPriority(spec_G7,sort_Note,1).

%% operators %%
hasOp(spec_G7,po_0,0).
opHasSort(spec_G7,po_0,sort_Note,domain,0).
isDataOp(spec_G7,po_0).
opHasPriority(spec_G7,po_0,1).

hasOp(spec_G7,po_1,0).
opHasSort(spec_G7,po_1,sort_Note,domain,0).
isDataOp(spec_G7,po_1).
opHasPriority(spec_G7,po_1,1).

hasOp(spec_G7,po_2,0).
opHasSort(spec_G7,po_2,sort_Note,domain,0).
isDataOp(spec_G7,po_2).
opHasPriority(spec_G7,po_2,1).

hasOp(spec_G7,po_3,0).
opHasSort(spec_G7,po_3,sort_Note,domain,0).
isDataOp(spec_G7,po_3).
opHasPriority(spec_G7,po_3,1).

hasOp(spec_G7,po_4,0).
opHasSort(spec_G7,po_4,sort_Note,domain,0).
isDataOp(spec_G7,po_4).
opHasPriority(spec_G7,po_4,1).

hasOp(spec_G7,po_5,0).
opHasSort(spec_G7,po_5,sort_Note,domain,0).
isDataOp(spec_G7,po_5).
opHasPriority(spec_G7,po_5,1).

hasOp(spec_G7,po_6,0).
opHasSort(spec_G7,po_6,sort_Note,domain,0).
isDataOp(spec_G7,po_6).
opHasPriority(spec_G7,po_6,1).

hasOp(spec_G7,po_7,0).
opHasSort(spec_G7,po_7,sort_Note,domain,0).
isDataOp(spec_G7,po_7).
opHasPriority(spec_G7,po_7,1).

hasOp(spec_G7,po_8,0).
opHasSort(spec_G7,po_8,sort_Note,domain,0).
isDataOp(spec_G7,po_8).
opHasPriority(spec_G7,po_8,1).

hasOp(spec_G7,po_9,0).
opHasSort(spec_G7,po_9,sort_Note,domain,0).
isDataOp(spec_G7,po_9).
opHasPriority(spec_G7,po_9,1).

hasOp(spec_G7,po_plus,0).
opHasSort(spec_G7,po_plus,sort_Note,arg1,0).
opHasSort(spec_G7,po_plus,sort_Note,arg2,0).
opHasSort(spec_G7,po_plus,sort_Note,domain,0).
isNonDataOp(spec_G7,po_plus).
opHasPriority(spec_G7,po_plus,1).

hasOp(spec_G7,po_c,0).
opHasSort(spec_G7,po_c,sort_Chord,domain,0).
isNonDataOp(spec_G7,po_c).
opHasPriority(spec_G7,po_c,1).

hasOp(spec_G7,po_root,0).
opHasSort(spec_G7,po_root,sort_Chord,arg1,0).
opHasSort(spec_G7,po_root,sort_Note,domain,0).
isNonDataOp(spec_G7,po_root).
opHasPriority(spec_G7,po_root,1).

hasOp(spec_G7,po_suc,0).
opHasSort(spec_G7,po_suc,sort_Note,arg1,0).
opHasSort(spec_G7,po_suc,sort_Note,domain,0).
isNonDataOp(spec_G7,po_suc).
opHasPriority(spec_G7,po_suc,1).

hasOp(spec_G7,po_x,0).
opHasSort(spec_G7,po_x,sort_Note,domain,0).
isDataOp(spec_G7,po_x).
opHasPriority(spec_G7,po_x,1).

hasOp(spec_G7,po_x1,0).
opHasSort(spec_G7,po_x1,sort_Note,domain,0).
isDataOp(spec_G7,po_x1).
opHasPriority(spec_G7,po_x1,1).

%% predicates %%
hasPred(spec_G7,po_hasAbsNote,0).
predHasSort(spec_G7,po_hasAbsNote,sort_Chord,arg1,0).
predHasSort(spec_G7,po_hasAbsNote,sort_Note,arg2,0).
predHasPriority(spec_G7,po_hasAbsNote,1).

hasPred(spec_G7,po_hasRelNote,0).
predHasSort(spec_G7,po_hasRelNote,sort_Chord,arg1,0).
predHasSort(spec_G7,po_hasRelNote,sort_Note,arg2,0).
predHasPriority(spec_G7,po_hasRelNote,1).

%% axioms %%

%% Axiom Ax-absg7:p:1 %%
hasAxiom(spec_G7,0,0).
axHasEquivalenceClass(spec_G7,0,0,0).
isNonDataAx(spec_G7,0).
axHasPriority(spec_G7,0,1).
axInvolvesPredOp(spec_G7,0,po_c,0).
axInvolvesPredOp(spec_G7,0,po_7,0).
axInvolvesPredOp(spec_G7,0,po_hasAbsNote,0).


%% Axiom Ax-absg11:p:3 %%
hasAxiom(spec_G7,1,0).
axHasEquivalenceClass(spec_G7,1,1,0).
isNonDataAx(spec_G7,1).
axHasPriority(spec_G7,1,3).
axInvolvesPredOp(spec_G7,1,po_c,0).
axInvolvesPredOp(spec_G7,1,po_x1,0).
axInvolvesPredOp(spec_G7,1,po_hasAbsNote,0).


%% Axiom Ax-absg2:p:1 %%
hasAxiom(spec_G7,2,0).
axHasEquivalenceClass(spec_G7,2,2,0).
isNonDataAx(spec_G7,2).
axHasPriority(spec_G7,2,1).
axInvolvesPredOp(spec_G7,2,po_c,0).
axInvolvesPredOp(spec_G7,2,po_2,0).
axInvolvesPredOp(spec_G7,2,po_hasAbsNote,0).


%% Axiom Ax-absg5:p:2 %%
hasAxiom(spec_G7,3,0).
axHasEquivalenceClass(spec_G7,3,3,0).
isNonDataAx(spec_G7,3).
axHasPriority(spec_G7,3,2).
axInvolvesPredOp(spec_G7,3,po_c,0).
axInvolvesPredOp(spec_G7,3,po_5,0).
axInvolvesPredOp(spec_G7,3,po_hasAbsNote,0).


%% Axiom Ax-rootg7:p:1 %%
hasAxiom(spec_G7,4,0).
axHasEquivalenceClass(spec_G7,4,4,0).
isNonDataAx(spec_G7,4).
axHasPriority(spec_G7,4,1).
axInvolvesPredOp(spec_G7,4,po_c,0).
axInvolvesPredOp(spec_G7,4,po_root,0).
axInvolvesPredOp(spec_G7,4,po_7,0).


%% Axiom Ax-relg0:p:3 %%
hasAxiom(spec_G7,5,0).
axHasEquivalenceClass(spec_G7,5,5,0).
isNonDataAx(spec_G7,5).
axHasPriority(spec_G7,5,3).
axInvolvesPredOp(spec_G7,5,po_0,0).
axInvolvesPredOp(spec_G7,5,po_c,0).
axInvolvesPredOp(spec_G7,5,po_hasRelNote,0).


%% Axiom Ax-relg4:p:3 %%
hasAxiom(spec_G7,6,0).
axHasEquivalenceClass(spec_G7,6,6,0).
isNonDataAx(spec_G7,6).
axHasPriority(spec_G7,6,3).
axInvolvesPredOp(spec_G7,6,po_c,0).
axInvolvesPredOp(spec_G7,6,po_4,0).
axInvolvesPredOp(spec_G7,6,po_hasRelNote,0).


%% Axiom Ax-relg7:p:0 %%
hasAxiom(spec_G7,7,0).
axHasEquivalenceClass(spec_G7,7,7,0).
isNonDataAx(spec_G7,7).
axHasPriority(spec_G7,7,0).
axInvolvesPredOp(spec_G7,7,po_c,0).
axInvolvesPredOp(spec_G7,7,po_7,0).
axInvolvesPredOp(spec_G7,7,po_hasRelNote,0).


%% Axiom Ax-relg10:p:2 %%
hasAxiom(spec_G7,8,0).
axHasEquivalenceClass(spec_G7,8,8,0).
isNonDataAx(spec_G7,8).
axHasPriority(spec_G7,8,2).
axInvolvesPredOp(spec_G7,8,po_x,0).
axInvolvesPredOp(spec_G7,8,po_c,0).
axInvolvesPredOp(spec_G7,8,po_hasRelNote,0).


%% Axiom Ax6 %%
hasAxiom(spec_G7,14,0).
axHasEquivalenceClass(spec_G7,14,14,0).
isNonDataAx(spec_G7,14).
axHasPriority(spec_G7,14,1).
axInvolvesPredOp(spec_G7,14,po_Chord,0).
axInvolvesPredOp(spec_G7,14,po_hasAbsNote,0).
axInvolvesPredOp(spec_G7,14,po_6,0).
axInvolvesPredOp(spec_G7,14,po_1,0).
axInvolvesPredOp(spec_G7,14,po_7,0).
axInvolvesPredOp(spec_G7,14,po_hasRelNote,0).
axInvolvesPredOp(spec_G7,14,po_var,0).
axInvolvesPredOp(spec_G7,14,po_root,0).
axInvolvesSort(spec_G7,14,sort_Chord,0).


%% Axiom AxSucc0:p:100:r:0 %%
hasAxiom(spec_G7,20,0).
axHasEquivalenceClass(spec_G7,20,20,0).
isNonDataAx(spec_G7,20).
axHasPriority(spec_G7,20,100).
axInvolvesPredOp(spec_G7,20,po_1,0).
axInvolvesPredOp(spec_G7,20,po_0,0).
axInvolvesPredOp(spec_G7,20,po_suc,0).


%% Axiom AxSucc1:p:100:r:0 %%
hasAxiom(spec_G7,21,0).
axHasEquivalenceClass(spec_G7,21,21,0).
isNonDataAx(spec_G7,21).
axHasPriority(spec_G7,21,100).
axInvolvesPredOp(spec_G7,21,po_1,0).
axInvolvesPredOp(spec_G7,21,po_2,0).
axInvolvesPredOp(spec_G7,21,po_suc,0).


%% Axiom AxSucc2:p:100:r:0 %%
hasAxiom(spec_G7,22,0).
axHasEquivalenceClass(spec_G7,22,22,0).
isNonDataAx(spec_G7,22).
axHasPriority(spec_G7,22,100).
axInvolvesPredOp(spec_G7,22,po_3,0).
axInvolvesPredOp(spec_G7,22,po_2,0).
axInvolvesPredOp(spec_G7,22,po_suc,0).


%% Axiom AxSucc3:p:100:r:0 %%
hasAxiom(spec_G7,23,0).
axHasEquivalenceClass(spec_G7,23,23,0).
isNonDataAx(spec_G7,23).
axHasPriority(spec_G7,23,100).
axInvolvesPredOp(spec_G7,23,po_3,0).
axInvolvesPredOp(spec_G7,23,po_suc,0).
axInvolvesPredOp(spec_G7,23,po_4,0).


%% Axiom AxSucc4:p:100:r:0 %%
hasAxiom(spec_G7,24,0).
axHasEquivalenceClass(spec_G7,24,24,0).
isNonDataAx(spec_G7,24).
axHasPriority(spec_G7,24,100).
axInvolvesPredOp(spec_G7,24,po_5,0).
axInvolvesPredOp(spec_G7,24,po_suc,0).
axInvolvesPredOp(spec_G7,24,po_4,0).


%% Axiom AxSucc5:p:100:r:0 %%
hasAxiom(spec_G7,25,0).
axHasEquivalenceClass(spec_G7,25,25,0).
isNonDataAx(spec_G7,25).
axHasPriority(spec_G7,25,100).
axInvolvesPredOp(spec_G7,25,po_5,0).
axInvolvesPredOp(spec_G7,25,po_suc,0).
axInvolvesPredOp(spec_G7,25,po_6,0).


%% Axiom AxSucc6:p:100:r:0 %%
hasAxiom(spec_G7,26,0).
axHasEquivalenceClass(spec_G7,26,26,0).
isNonDataAx(spec_G7,26).
axHasPriority(spec_G7,26,100).
axInvolvesPredOp(spec_G7,26,po_suc,0).
axInvolvesPredOp(spec_G7,26,po_7,0).
axInvolvesPredOp(spec_G7,26,po_6,0).


%% Axiom AxSucc7:p:100:r:0 %%
hasAxiom(spec_G7,27,0).
axHasEquivalenceClass(spec_G7,27,27,0).
isNonDataAx(spec_G7,27).
axHasPriority(spec_G7,27,100).
axInvolvesPredOp(spec_G7,27,po_8,0).
axInvolvesPredOp(spec_G7,27,po_suc,0).
axInvolvesPredOp(spec_G7,27,po_7,0).


%% Axiom AxSucc8:p:100:r:0 %%
hasAxiom(spec_G7,28,0).
axHasEquivalenceClass(spec_G7,28,28,0).
isNonDataAx(spec_G7,28).
axHasPriority(spec_G7,28,100).
axInvolvesPredOp(spec_G7,28,po_9,0).
axInvolvesPredOp(spec_G7,28,po_8,0).
axInvolvesPredOp(spec_G7,28,po_suc,0).


%% Axiom AxSucc9:p:100:r:0 %%
hasAxiom(spec_G7,29,0).
axHasEquivalenceClass(spec_G7,29,29,0).
isNonDataAx(spec_G7,29).
axHasPriority(spec_G7,29,100).
axInvolvesPredOp(spec_G7,29,po_9,0).
axInvolvesPredOp(spec_G7,29,po_x,0).
axInvolvesPredOp(spec_G7,29,po_suc,0).


%% Axiom AxSucc10:p:100:r:0 %%
hasAxiom(spec_G7,30,0).
axHasEquivalenceClass(spec_G7,30,30,0).
isNonDataAx(spec_G7,30).
axHasPriority(spec_G7,30,100).
axInvolvesPredOp(spec_G7,30,po_x,0).
axInvolvesPredOp(spec_G7,30,po_x1,0).
axInvolvesPredOp(spec_G7,30,po_suc,0).


%% Axiom AxSucc11:p:100:r:0 %%
hasAxiom(spec_G7,31,0).
axHasEquivalenceClass(spec_G7,31,31,0).
isNonDataAx(spec_G7,31).
axHasPriority(spec_G7,31,100).
axInvolvesPredOp(spec_G7,31,po_0,0).
axInvolvesPredOp(spec_G7,31,po_x1,0).
axInvolvesPredOp(spec_G7,31,po_suc,0).


%% Axiom AxSucc0-11:p:100:r:0 %%
hasAxiom(spec_G7,43,0).
axHasEquivalenceClass(spec_G7,43,43,0).
isNonDataAx(spec_G7,43).
axHasPriority(spec_G7,43,100).
axInvolvesPredOp(spec_G7,43,po_var,0).
axInvolvesPredOp(spec_G7,43,po_0,0).
axInvolvesPredOp(spec_G7,43,po_x1,0).
axInvolvesPredOp(spec_G7,43,po_suc,0).
axInvolvesPredOp(spec_G7,43,po_Note,0).
axInvolvesSort(spec_G7,43,sort_Note,0).


%% Axiom AxSuccEql:p:100:r:0 %%
hasAxiom(spec_G7,44,0).
axHasEquivalenceClass(spec_G7,44,44,0).
isNonDataAx(spec_G7,44).
axHasPriority(spec_G7,44,100).
axInvolvesPredOp(spec_G7,44,po_var,0).
axInvolvesPredOp(spec_G7,44,po_Note,0).
axInvolvesPredOp(spec_G7,44,po_suc,0).
axInvolvesSort(spec_G7,44,sort_Note,0).


%% Axiom AxSuccDontKnow:p:100:r:0 %%
hasAxiom(spec_G7,45,0).
axHasEquivalenceClass(spec_G7,45,45,0).
isNonDataAx(spec_G7,45).
axHasPriority(spec_G7,45,100).
axInvolvesPredOp(spec_G7,45,po_var,0).
axInvolvesPredOp(spec_G7,45,po_0,0).
axInvolvesPredOp(spec_G7,45,po_plus,0).
axInvolvesPredOp(spec_G7,45,po_Note,0).
axInvolvesSort(spec_G7,45,sort_Note,0).


%% Axiom AxSuccAdd:p:100:r:0 %%
hasAxiom(spec_G7,46,0).
axHasEquivalenceClass(spec_G7,46,46,0).
isNonDataAx(spec_G7,46).
axHasPriority(spec_G7,46,100).
axInvolvesPredOp(spec_G7,46,po_plus,0).
axInvolvesPredOp(spec_G7,46,po_suc,0).
axInvolvesSort(spec_G7,46,sort_Note,0).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% spec Bbmin %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spec(spec_Bbmin).
hasId(spec_Bbmin,1).

%% sorts %%
hasSort(spec_Bbmin,sort_Chord,0).
isNonDataSort(spec_Bbmin,sort_Chord).
sortHasPriority(spec_Bbmin,sort_Chord,1).

hasSort(spec_Bbmin,sort_Note,0).
isDataSort(spec_Bbmin,sort_Note).
sortHasPriority(spec_Bbmin,sort_Note,1).

%% operators %%
hasOp(spec_Bbmin,po_0,0).
opHasSort(spec_Bbmin,po_0,sort_Note,domain,0).
isDataOp(spec_Bbmin,po_0).
opHasPriority(spec_Bbmin,po_0,1).

hasOp(spec_Bbmin,po_1,0).
opHasSort(spec_Bbmin,po_1,sort_Note,domain,0).
isDataOp(spec_Bbmin,po_1).
opHasPriority(spec_Bbmin,po_1,1).

hasOp(spec_Bbmin,po_2,0).
opHasSort(spec_Bbmin,po_2,sort_Note,domain,0).
isDataOp(spec_Bbmin,po_2).
opHasPriority(spec_Bbmin,po_2,1).

hasOp(spec_Bbmin,po_3,0).
opHasSort(spec_Bbmin,po_3,sort_Note,domain,0).
isDataOp(spec_Bbmin,po_3).
opHasPriority(spec_Bbmin,po_3,1).

hasOp(spec_Bbmin,po_4,0).
opHasSort(spec_Bbmin,po_4,sort_Note,domain,0).
isDataOp(spec_Bbmin,po_4).
opHasPriority(spec_Bbmin,po_4,1).

hasOp(spec_Bbmin,po_5,0).
opHasSort(spec_Bbmin,po_5,sort_Note,domain,0).
isDataOp(spec_Bbmin,po_5).
opHasPriority(spec_Bbmin,po_5,1).

hasOp(spec_Bbmin,po_6,0).
opHasSort(spec_Bbmin,po_6,sort_Note,domain,0).
isDataOp(spec_Bbmin,po_6).
opHasPriority(spec_Bbmin,po_6,1).

hasOp(spec_Bbmin,po_7,0).
opHasSort(spec_Bbmin,po_7,sort_Note,domain,0).
isDataOp(spec_Bbmin,po_7).
opHasPriority(spec_Bbmin,po_7,1).

hasOp(spec_Bbmin,po_8,0).
opHasSort(spec_Bbmin,po_8,sort_Note,domain,0).
isDataOp(spec_Bbmin,po_8).
opHasPriority(spec_Bbmin,po_8,1).

hasOp(spec_Bbmin,po_9,0).
opHasSort(spec_Bbmin,po_9,sort_Note,domain,0).
isDataOp(spec_Bbmin,po_9).
opHasPriority(spec_Bbmin,po_9,1).

hasOp(spec_Bbmin,po_plus,0).
opHasSort(spec_Bbmin,po_plus,sort_Note,arg1,0).
opHasSort(spec_Bbmin,po_plus,sort_Note,arg2,0).
opHasSort(spec_Bbmin,po_plus,sort_Note,domain,0).
isNonDataOp(spec_Bbmin,po_plus).
opHasPriority(spec_Bbmin,po_plus,1).

hasOp(spec_Bbmin,po_c,0).
opHasSort(spec_Bbmin,po_c,sort_Chord,domain,0).
isNonDataOp(spec_Bbmin,po_c).
opHasPriority(spec_Bbmin,po_c,1).

hasOp(spec_Bbmin,po_root,0).
opHasSort(spec_Bbmin,po_root,sort_Chord,arg1,0).
opHasSort(spec_Bbmin,po_root,sort_Note,domain,0).
isNonDataOp(spec_Bbmin,po_root).
opHasPriority(spec_Bbmin,po_root,1).

hasOp(spec_Bbmin,po_suc,0).
opHasSort(spec_Bbmin,po_suc,sort_Note,arg1,0).
opHasSort(spec_Bbmin,po_suc,sort_Note,domain,0).
isNonDataOp(spec_Bbmin,po_suc).
opHasPriority(spec_Bbmin,po_suc,1).

hasOp(spec_Bbmin,po_x,0).
opHasSort(spec_Bbmin,po_x,sort_Note,domain,0).
isDataOp(spec_Bbmin,po_x).
opHasPriority(spec_Bbmin,po_x,1).

hasOp(spec_Bbmin,po_x1,0).
opHasSort(spec_Bbmin,po_x1,sort_Note,domain,0).
isDataOp(spec_Bbmin,po_x1).
opHasPriority(spec_Bbmin,po_x1,1).

%% predicates %%
hasPred(spec_Bbmin,po_hasAbsNote,0).
predHasSort(spec_Bbmin,po_hasAbsNote,sort_Chord,arg1,0).
predHasSort(spec_Bbmin,po_hasAbsNote,sort_Note,arg2,0).
predHasPriority(spec_Bbmin,po_hasAbsNote,1).

hasPred(spec_Bbmin,po_hasRelNote,0).
predHasSort(spec_Bbmin,po_hasRelNote,sort_Chord,arg1,0).
predHasSort(spec_Bbmin,po_hasRelNote,sort_Note,arg2,0).
predHasPriority(spec_Bbmin,po_hasRelNote,1).

%% axioms %%

%% Axiom Ax-absb10:p:1 %%
hasAxiom(spec_Bbmin,47,0).
axHasEquivalenceClass(spec_Bbmin,47,47,0).
isNonDataAx(spec_Bbmin,47).
axHasPriority(spec_Bbmin,47,1).
axInvolvesPredOp(spec_Bbmin,47,po_x,0).
axInvolvesPredOp(spec_Bbmin,47,po_c,0).
axInvolvesPredOp(spec_Bbmin,47,po_hasAbsNote,0).


%% Axiom Ax-absb1:p:3 %%
hasAxiom(spec_Bbmin,48,0).
axHasEquivalenceClass(spec_Bbmin,48,48,0).
isNonDataAx(spec_Bbmin,48).
axHasPriority(spec_Bbmin,48,3).
axInvolvesPredOp(spec_Bbmin,48,po_1,0).
axInvolvesPredOp(spec_Bbmin,48,po_c,0).
axInvolvesPredOp(spec_Bbmin,48,po_hasAbsNote,0).


%% Axiom Ax-absb5:p:2 %%
hasAxiom(spec_Bbmin,49,0).
axHasEquivalenceClass(spec_Bbmin,49,3,0).
isNonDataAx(spec_Bbmin,49).
axHasPriority(spec_Bbmin,49,2).
axInvolvesPredOp(spec_Bbmin,49,po_c,0).
axInvolvesPredOp(spec_Bbmin,49,po_5,0).
axInvolvesPredOp(spec_Bbmin,49,po_hasAbsNote,0).


%% Axiom Ax-rootb10:p:2 %%
hasAxiom(spec_Bbmin,50,0).
axHasEquivalenceClass(spec_Bbmin,50,49,0).
isNonDataAx(spec_Bbmin,50).
axHasPriority(spec_Bbmin,50,2).
axInvolvesPredOp(spec_Bbmin,50,po_x,0).
axInvolvesPredOp(spec_Bbmin,50,po_c,0).
axInvolvesPredOp(spec_Bbmin,50,po_root,0).


%% Axiom Ax-relb0:p:2 %%
hasAxiom(spec_Bbmin,51,0).
axHasEquivalenceClass(spec_Bbmin,51,5,0).
isNonDataAx(spec_Bbmin,51).
axHasPriority(spec_Bbmin,51,2).
axInvolvesPredOp(spec_Bbmin,51,po_0,0).
axInvolvesPredOp(spec_Bbmin,51,po_c,0).
axInvolvesPredOp(spec_Bbmin,51,po_hasRelNote,0).


%% Axiom Ax-relb3:p:1 %%
hasAxiom(spec_Bbmin,52,0).
axHasEquivalenceClass(spec_Bbmin,52,50,0).
isNonDataAx(spec_Bbmin,52).
axHasPriority(spec_Bbmin,52,1).
axInvolvesPredOp(spec_Bbmin,52,po_c,0).
axInvolvesPredOp(spec_Bbmin,52,po_3,0).
axInvolvesPredOp(spec_Bbmin,52,po_hasRelNote,0).


%% Axiom Ax-relbb7:p:0 %%
hasAxiom(spec_Bbmin,53,0).
axHasEquivalenceClass(spec_Bbmin,53,7,0).
isNonDataAx(spec_Bbmin,53).
axHasPriority(spec_Bbmin,53,0).
axInvolvesPredOp(spec_Bbmin,53,po_c,0).
axInvolvesPredOp(spec_Bbmin,53,po_7,0).
axInvolvesPredOp(spec_Bbmin,53,po_hasRelNote,0).


%% Axiom Ax6 %%
hasAxiom(spec_Bbmin,59,0).
axHasEquivalenceClass(spec_Bbmin,59,14,0).
isNonDataAx(spec_Bbmin,59).
axHasPriority(spec_Bbmin,59,1).
axInvolvesPredOp(spec_Bbmin,59,po_Chord,0).
axInvolvesPredOp(spec_Bbmin,59,po_hasAbsNote,0).
axInvolvesPredOp(spec_Bbmin,59,po_6,0).
axInvolvesPredOp(spec_Bbmin,59,po_1,0).
axInvolvesPredOp(spec_Bbmin,59,po_7,0).
axInvolvesPredOp(spec_Bbmin,59,po_hasRelNote,0).
axInvolvesPredOp(spec_Bbmin,59,po_var,0).
axInvolvesPredOp(spec_Bbmin,59,po_root,0).
axInvolvesSort(spec_Bbmin,59,sort_Chord,0).


%% Axiom AxSucc0:p:100:r:0 %%
hasAxiom(spec_Bbmin,65,0).
axHasEquivalenceClass(spec_Bbmin,65,20,0).
isNonDataAx(spec_Bbmin,65).
axHasPriority(spec_Bbmin,65,100).
axInvolvesPredOp(spec_Bbmin,65,po_1,0).
axInvolvesPredOp(spec_Bbmin,65,po_0,0).
axInvolvesPredOp(spec_Bbmin,65,po_suc,0).


%% Axiom AxSucc1:p:100:r:0 %%
hasAxiom(spec_Bbmin,66,0).
axHasEquivalenceClass(spec_Bbmin,66,21,0).
isNonDataAx(spec_Bbmin,66).
axHasPriority(spec_Bbmin,66,100).
axInvolvesPredOp(spec_Bbmin,66,po_1,0).
axInvolvesPredOp(spec_Bbmin,66,po_2,0).
axInvolvesPredOp(spec_Bbmin,66,po_suc,0).


%% Axiom AxSucc2:p:100:r:0 %%
hasAxiom(spec_Bbmin,67,0).
axHasEquivalenceClass(spec_Bbmin,67,22,0).
isNonDataAx(spec_Bbmin,67).
axHasPriority(spec_Bbmin,67,100).
axInvolvesPredOp(spec_Bbmin,67,po_3,0).
axInvolvesPredOp(spec_Bbmin,67,po_2,0).
axInvolvesPredOp(spec_Bbmin,67,po_suc,0).


%% Axiom AxSucc3:p:100:r:0 %%
hasAxiom(spec_Bbmin,68,0).
axHasEquivalenceClass(spec_Bbmin,68,23,0).
isNonDataAx(spec_Bbmin,68).
axHasPriority(spec_Bbmin,68,100).
axInvolvesPredOp(spec_Bbmin,68,po_3,0).
axInvolvesPredOp(spec_Bbmin,68,po_suc,0).
axInvolvesPredOp(spec_Bbmin,68,po_4,0).


%% Axiom AxSucc4:p:100:r:0 %%
hasAxiom(spec_Bbmin,69,0).
axHasEquivalenceClass(spec_Bbmin,69,24,0).
isNonDataAx(spec_Bbmin,69).
axHasPriority(spec_Bbmin,69,100).
axInvolvesPredOp(spec_Bbmin,69,po_5,0).
axInvolvesPredOp(spec_Bbmin,69,po_suc,0).
axInvolvesPredOp(spec_Bbmin,69,po_4,0).


%% Axiom AxSucc5:p:100:r:0 %%
hasAxiom(spec_Bbmin,70,0).
axHasEquivalenceClass(spec_Bbmin,70,25,0).
isNonDataAx(spec_Bbmin,70).
axHasPriority(spec_Bbmin,70,100).
axInvolvesPredOp(spec_Bbmin,70,po_5,0).
axInvolvesPredOp(spec_Bbmin,70,po_suc,0).
axInvolvesPredOp(spec_Bbmin,70,po_6,0).


%% Axiom AxSucc6:p:100:r:0 %%
hasAxiom(spec_Bbmin,71,0).
axHasEquivalenceClass(spec_Bbmin,71,26,0).
isNonDataAx(spec_Bbmin,71).
axHasPriority(spec_Bbmin,71,100).
axInvolvesPredOp(spec_Bbmin,71,po_suc,0).
axInvolvesPredOp(spec_Bbmin,71,po_7,0).
axInvolvesPredOp(spec_Bbmin,71,po_6,0).


%% Axiom AxSucc7:p:100:r:0 %%
hasAxiom(spec_Bbmin,72,0).
axHasEquivalenceClass(spec_Bbmin,72,27,0).
isNonDataAx(spec_Bbmin,72).
axHasPriority(spec_Bbmin,72,100).
axInvolvesPredOp(spec_Bbmin,72,po_8,0).
axInvolvesPredOp(spec_Bbmin,72,po_suc,0).
axInvolvesPredOp(spec_Bbmin,72,po_7,0).


%% Axiom AxSucc8:p:100:r:0 %%
hasAxiom(spec_Bbmin,73,0).
axHasEquivalenceClass(spec_Bbmin,73,28,0).
isNonDataAx(spec_Bbmin,73).
axHasPriority(spec_Bbmin,73,100).
axInvolvesPredOp(spec_Bbmin,73,po_9,0).
axInvolvesPredOp(spec_Bbmin,73,po_8,0).
axInvolvesPredOp(spec_Bbmin,73,po_suc,0).


%% Axiom AxSucc9:p:100:r:0 %%
hasAxiom(spec_Bbmin,74,0).
axHasEquivalenceClass(spec_Bbmin,74,29,0).
isNonDataAx(spec_Bbmin,74).
axHasPriority(spec_Bbmin,74,100).
axInvolvesPredOp(spec_Bbmin,74,po_9,0).
axInvolvesPredOp(spec_Bbmin,74,po_x,0).
axInvolvesPredOp(spec_Bbmin,74,po_suc,0).


%% Axiom AxSucc10:p:100:r:0 %%
hasAxiom(spec_Bbmin,75,0).
axHasEquivalenceClass(spec_Bbmin,75,30,0).
isNonDataAx(spec_Bbmin,75).
axHasPriority(spec_Bbmin,75,100).
axInvolvesPredOp(spec_Bbmin,75,po_x,0).
axInvolvesPredOp(spec_Bbmin,75,po_x1,0).
axInvolvesPredOp(spec_Bbmin,75,po_suc,0).


%% Axiom AxSucc11:p:100:r:0 %%
hasAxiom(spec_Bbmin,76,0).
axHasEquivalenceClass(spec_Bbmin,76,31,0).
isNonDataAx(spec_Bbmin,76).
axHasPriority(spec_Bbmin,76,100).
axInvolvesPredOp(spec_Bbmin,76,po_0,0).
axInvolvesPredOp(spec_Bbmin,76,po_x1,0).
axInvolvesPredOp(spec_Bbmin,76,po_suc,0).


%% Axiom AxSucc0-11:p:100:r:0 %%
hasAxiom(spec_Bbmin,88,0).
axHasEquivalenceClass(spec_Bbmin,88,43,0).
isNonDataAx(spec_Bbmin,88).
axHasPriority(spec_Bbmin,88,100).
axInvolvesPredOp(spec_Bbmin,88,po_var,0).
axInvolvesPredOp(spec_Bbmin,88,po_0,0).
axInvolvesPredOp(spec_Bbmin,88,po_x1,0).
axInvolvesPredOp(spec_Bbmin,88,po_suc,0).
axInvolvesPredOp(spec_Bbmin,88,po_Note,0).
axInvolvesSort(spec_Bbmin,88,sort_Note,0).


%% Axiom AxSuccEql:p:100:r:0 %%
hasAxiom(spec_Bbmin,89,0).
axHasEquivalenceClass(spec_Bbmin,89,44,0).
isNonDataAx(spec_Bbmin,89).
axHasPriority(spec_Bbmin,89,100).
axInvolvesPredOp(spec_Bbmin,89,po_var,0).
axInvolvesPredOp(spec_Bbmin,89,po_Note,0).
axInvolvesPredOp(spec_Bbmin,89,po_suc,0).
axInvolvesSort(spec_Bbmin,89,sort_Note,0).


%% Axiom AxSuccDontKnow:p:100:r:0 %%
hasAxiom(spec_Bbmin,90,0).
axHasEquivalenceClass(spec_Bbmin,90,45,0).
isNonDataAx(spec_Bbmin,90).
axHasPriority(spec_Bbmin,90,100).
axInvolvesPredOp(spec_Bbmin,90,po_var,0).
axInvolvesPredOp(spec_Bbmin,90,po_0,0).
axInvolvesPredOp(spec_Bbmin,90,po_plus,0).
axInvolvesPredOp(spec_Bbmin,90,po_Note,0).
axInvolvesSort(spec_Bbmin,90,sort_Note,0).


%% Axiom AxSuccAdd:p:100:r:0 %%
hasAxiom(spec_Bbmin,91,0).
axHasEquivalenceClass(spec_Bbmin,91,46,0).
isNonDataAx(spec_Bbmin,91).
axHasPriority(spec_Bbmin,91,100).
axInvolvesPredOp(spec_Bbmin,91,po_plus,0).
axInvolvesPredOp(spec_Bbmin,91,po_suc,0).
axInvolvesSort(spec_Bbmin,91,sort_Note,0).




