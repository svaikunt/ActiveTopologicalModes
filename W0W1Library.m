(* ::Package:: *)

(*EtIn1[pos_,Nt_,Nto_,s2_,Ea_]:=(s2-Ea)*Tanh[(-Nto*Quotient[pos,2]*2+Ea*Nt)]*)
(*Ladder Netwrok*)
(*Function that defines rate of hop from state j to state i in the ladder network. Number of nodes in network is 2 Nt. Ea is typically set to zero *) 
(*Very important, Q are the chemical potential terms*) 
(*s2 controls b rates*)
(*t1 and t2 are like x1 and y1, b1 and b2 are like x2 and y2*)
MatrixElementRectangleAllLinksRotJ= Compile[{{i, _Integer}, {j, _Integer}, {s1, _Real}, {s2, _Real}, {t1, _Real},{b1, _Real},{Ea,_Real},{Nt,_Real},{Q,_Complex}},
		If [EvenQ[i],
			If[j-1 == i,   s1 RandomReal[{1-Q,1+Q}] (*Even to Odd *), 
				If[j==i-2,   t1*RandomReal[{1-Q,1+Q}] , (*m to m+1*)
			If[j==i+2, t1 RandomReal[{1-Q,1+Q}] ,If[j==i,-2*t1*RandomReal[{1-Q,1+Q}]-1*RandomReal[{1-Q,1+Q}],0]]]],(*m+1 to m*)(*Odd to Even*)
			If [
				j == i+2, s2 RandomReal[{1-Q,1+Q}] ,  
				If[j==i-2,  s2   RandomReal[{1-Q,1+Q}], 
					If[j==i+1,  -b1 RandomReal[{1-Q,1+Q}] , 
						If[j==i-3, -b1 RandomReal[{1-Q,1+Q}],
							If[j==i-1,2*b1 RandomReal[{1-Q,1+Q}],
								If[j==i,-2*s2*RandomReal[{1-Q,1+Q}]-Ea RandomReal[{1-Q,1+Q}],0]]]]]]]
];
(*Subscript[\[Lambda], \[CapitalOmega]]\[Equal]t1, a= s1 ,b=b1,Ea=Subscript[\[Lambda], \[Omega]],s2=1*)




(*Defines transition matrix with Q's built in*)
WRectangleAllLinksRotJ[s1_, s2_, t1_,  b1_,  h1_,h2_,h3_,h4_,Ea_,Nt_,Q_] := Module[
	{Wtemp, s, Nsites,Wtempdiag,tempRandom},
	Nsites = 2 * Nt ;
    SeedRandom[];
Wtemp=Table[MatrixElementRectangleAllLinksRotJ[i, j, s1, s2,t1, b1 ,Ea,Nt,Q], {i, 1, Nsites}, {j, 1, Nsites}];
(*Wtemp=SparseArray[{{i_,j_}->MatrixElementRectangleAllLinksSqJ[i, j, s1, s2,t1, b1, b2,Ea,El,Nt,Q]}];*)
Wtemp[[1]][[3]]=0;
Wtemp[[1]][[All]]=0;
(*Wtemp[[2]][[All]]=0;*)
Wtemp[[2]][[All]]=0;
Wtemp[[Nsites]][[All]]=0;
(*Wtemp[[All]][[Nsites]]=0;*)
Wtemp[[Nsites-1]][[All]]=0;
(*Wtemp[[All]][[Nsites-1]]=0;*)

Wtemp[[1]][[Nsites-1]] = h3 Exp[-I Q];
Wtemp[[Nsites-1]][[1]] = h4 Exp[I Q];
Wtemp[[2]][[Nsites]] = h1 Exp[-I Q];
Wtemp[[Nsites]][[2]] = h2 Exp[I Q];
(*Do[{If[EvenQ[i],Wtemp[i][i-1]=s1*Exp[-(Ea*(-1)*(-Nt*0.5+i/2.0))]],Wtemp[i+1][i]=s2*Exp[-(Ea*1*(-Nt*0.5+i/2.0))]},{i,1,Nsites}];*)
	Wtemp
];
