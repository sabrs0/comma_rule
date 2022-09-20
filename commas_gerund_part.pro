/*****************************************************************************

		Copyright (c) My Company

 Project:  TEST
 FileName: COMMAS_GERUND_PART.PRO
 Purpose: No description
 Written by: Visual Prolog
 Comments:
******************************************************************************/

include "test.inc"

domains
stringlist=string*
str = string
list = str* 
list_start = str*
list_stop = str*
face = integer
num = integer % 0 - ед, 1 - мн.
gender = integer % 0 - неопределен, 1 - м, 2 - ж.
padezh = integer % 1-6
predicates
%TEST
test_sentence(string)
test_partc(string, list)
find_partc(list, list)
%¬—ѕќћќ√
my_concat(list, string)
split(string, list)
universal(list, list)
sentence(list, list)
%solve(string, string)
%„ј—“» –≈„»
union(list, list)
union_unq(list, list)
noun(list, list, padezh, num, gender)
mest(list, list, padezh, num, gender)
verb(list, list, num, gender)
participle(list, list, padezh, num, gender)
gerund(list, list)
pretext(list, list)
adj(list, list)
adv(list, list)


% ќћЅќ
adv_precombo(list, list)
adv_combo(list, list)
adj_combo(list, list)
partc_combo(list, list, padezh, num, gender)
ger_combo(list, list)
%‘–ј«џ
adj_phrase(list, list)
partc_phrase(list, list, padezh, num, gender)
ger_phrase(list, list)

noun_phrase(list, list, padezh,num, gender)
mest_phrase(list, list,padezh,num, gender)
verb_phrase(list, list)
%ќЅќ–ќ“џ
ger_ob(list, list)
partc_ob(list, list,padezh, num, gender)
%√–”ѕѕџ
noun_group(list, list)
verb_group(list, list)
%ѕ–ј¬»Ћј
comma_rule(list, list)
%sentence(list, list)
clauses

my_concat([], "") :-!.
my_concat([Head | Tail], Res2Str) :-  my_concat(Tail, ResStr), concat(Head, " ", Head2), concat(Head2, ResStr, Res2Str). 

split(S, [H|T]) :-
  fronttoken(S, H, R),
  !,
  split(R, T).
split(_, []).
%чтобы найти соединение частей речи в оборотах(возможно надо будет переписать дл€ каждой части речи с отсечением)
universal(S1, S2) :- union(S1, S2);
					noun(S1, S2,_,_,_);
					verb(S1, S2,_,_);
					participle(S1, S2,_, _,_);
					gerund(S1, S2);
					pretext(S1, S2);
					adj(S1, S2);
					adv(S1, S2).

	%ѕ–ј¬»Ћј
	comma_rule([], []):-!.
	%comma_rule([Head |[S| Tail] ], OutList) :- gerund([S|Tail], Tail), OutList = [Head |[","|[S |[","| RecList]]]], comma_rule(Tail, RecList), !.
	%comma_rule([S| Tail], OutList) :- gerund([S|Tail], Tail), OutList = [S |[","| RecList]], comma_rule(Tail, RecList), !.
	
	comma_rule([Head |[S|[S2| Tail]] ], OutList) :- union([S|[S2| Tail]], [S2| Tail]), union([S2| Tail], Tail),
						OutList = [Head |[S|[S2|RecList]]], comma_rule(Tail, RecList), !.
	comma_rule([Head |[S| Tail] ], OutList) :- union([S|Tail], Tail), OutList = [Head |[","|[S | RecList]]], comma_rule(Tail, RecList), !.

	comma_rule([Head |[S| Tail] ], OutList) :- union([S|Tail], Tail), OutList = [Head |[","|[S | RecList]]], comma_rule(Tail, RecList), !.
	comma_rule([Head|Tail], OutList) :- OutList = [Head|RecList], comma_rule(Tail, RecList).
	
	%sentence([], []) :- !.
	%sentence([Head|Tail], OutList) :- OutList = [Head|RecList], noun_phrase(Tail, RecList).
	
	
	% ќћЅ»Ќј÷»»
	adv_precombo(S1, S3) :- adv(S1, S2), adv(S2, S3).
	adv_combo( S1, S3) :- pretext(S1, S2), adv_precombo(S2, S3);
						pretext(S1, S2), adv(S2, S3);
						adv(S1, S2), adv(S2, S3).
						
	adj_combo(S1, S3) :- pretext(S1, S2), adj(S2, S3);
						adv_combo(S1, S2), adj(S2, S3).
	
	partc_combo(S1, S3, Padezh, Num, Gender) :- adv_combo(S1, S2), participle(S2, S3, Padezh,Num,Gender);
										participle(S1, S2, Padezh,Num, Gender), adv_combo(S2, S3);
										pretext(S1, S2), participle(S2, S3, Padezh,Num, Gender).
	ger_combo(S1, S3) :- adv_combo(S1, S2), gerund(S2, S3);
						gerund(S1,S2), adv_combo(S2, S3).
	
	%‘–ј«џ
	%
	adj_phrase(S_Start, S_Stop) :-  adv_combo(S_Start, S2), adj(S2, S_Stop);
																adj_combo(S_Start, S2), adv_combo(S2, S_Stop);
																adv_combo(S_Start, S2), adj(S2, S_Stop);
																adj_combo(S_Start, S2), adv(S2, S_Stop);
																adv_precombo(S_Start, S2), adj(S2, S_Stop);
																adj(S_Start, S1), adv_precombo(S1, S_Stop);
																adv(S_Start, S2),  adj(S2, S_Stop);
																adj(S_Start, S2),adv(S2, S_Stop);
																pretext(S_Start, S2), adj(S2, S_Stop).
	
	%
	partc_phrase(S_Start, S_Stop,Padezh,Num,Gender) :-    adv_combo(S_Start, S2), participle(S2, S_Stop, Padezh,Num, Gender);
										partc_combo(S_Start, S2, Padezh,Num,Gender), adv_combo(S2, S_Stop);
										partc_combo(S_Start, S2, Padezh,Num,Gender),  adv(S2, S_Stop);
										adv_precombo(S_Start, S2), participle(S2, S_Stop, Padezh,Num, Gender);
										participle(S_Start, S2, Padezh,Num,Gender), adv_precombo(S2, S_Stop);
										adv(S_Start, S2),  participle(S2, S_Stop, Padezh,Num,Gender);
										participle(S_Start, S2, Padezh,Num,Gender), adv(S2, S_Stop);
										pretext(S_Start, S2),  participle(S2, S_Stop, Padezh,Num,Gender).
	
	%
	ger_phrase(S_Start, S_Stop) :-   adv_precombo(S_Start, S2), gerund(S2, S_Stop);
							gerund(S_Start, S2), adv_precombo(S2, S_Stop);
							adv(S_Start, S2), gerund(S2, S_Stop);
							gerund(S_Start, S2), adv(S2, S_Stop).
	%
	noun_phrase(S_Start, S_Stop,Padezh,Num,Gender) :- 
										adj_phrase(S_Start, S2), noun(S2, S_Stop, Padezh,Num,Gender);
										pretext(S_Start, S2), noun(S2, S_Stop, Padezh,Num,Gender);
										adj(S_Start, S2), noun(S2, S_Stop, Padezh,Num,Gender);
								    		noun(S_Start, S2, Padezh,Num,Gender), adj(S2, S_Stop);
								    		noun(S_Start, S2, Padezh,Num,Gender), adv(S2, S_Stop);
								    		adv(S_Start, S2), noun(S2, S_Stop, Padezh,Num,Gender);
								    		noun(S_Start, S_Stop, Padezh,Num,Gender).%, S_Stop = S_Start.
	
	%
	mest_phrase(S_Start, S_Stop,Padezh,Num,Gender) :- 
										adj_phrase(S_Start, S2), mest(S2, S_Stop, Padezh,Num,Gender);
										pretext(S_Start, S2), mest(S2, S_Stop, Padezh,Num,Gender);
										adj(S_Start, S2), mest(S2, S_Stop, Padezh,Num,Gender);
								    		mest(S_Start, S2, Padezh,Num,Gender), adj(S2, S_Stop);
								    		mest(S_Start, S2, Padezh,Num,Gender), adv(S2, S_Stop);
								    		adv(S_Start, S2), mest(S2, S_Stop, Padezh,Num,Gender);
								    		mest(S_Start, S_Stop, Padezh,Num,Gender).%, S_Stop = S_Start.
	%							  
	verb_phrase(S_Start, S_Stop) :-adv_precombo(S_Start, S2), verb(S2, S_Stop,_,_);
										verb(S_Start, S1,_,_), adv_precombo(S1, S_Stop);
										adv(S_Start, S1), verb(S1, S_Stop,_,_);
										verb(S_Start, S1,_,_), adv(S1, S_Stop);
										verb(S_Start,  S_Stop,_,_).%, S_Stop = S_Start.
										
	
	
	
	%ќЅќ–ќ“џ
	%
	ger_ob(S_Start, S_Stop) :- 
								noun_phrase(S_Start, S2, _, _, _),
								ger_phrase(S2,S_Stop);
								
								ger_phrase(S_Start, S2),
								noun_phrase(S2,S_Stop, _, _, _);
								
								ger_phrase(S_Start,S_Stop);
								
								gerund(S_Start, S_Stop);%, S_Stop = S_Start;
								
								mest_phrase(S_Start, S2, _, _, _),
								ger_phrase(S2,S_Stop);
								
								ger_phrase(S_Start, S2),
								mest_phrase(S2,S_Stop, _, _, _);
								
								ger_phrase(S_Start,S_Stop);
								
								gerund(S_Start, S_Stop).%, S_Stop = S_Start.
	
	%											
	partc_ob(S_Start, S_Stop,Padezh,Num,Gender) :- 
												noun_phrase(S_Start,S2,Padezh_Noun,Num_Noun,Gender_Noun),
												partc_phrase(S2,S_Stop,Padezh,Num,Gender), Num <> Num_Noun;
												
												noun_phrase(S_Start,S2,Padezh_Noun,Num_Noun,Gender_Noun),
												partc_phrase(S2,S_Stop,Padezh,Num,Gender), Gender <> Gender_Noun;
												
												noun_phrase(S_Start,S2,Padezh_Noun,Num_Noun,Gender_Noun),
												partc_phrase(S2,S_Stop,Padezh,Num,Gender), Padezh <> Padezh_Noun;
												
												partc_phrase( S_Start, S2,Padezh,Num,Gender),
												noun_phrase(S2,S_Stop,Padezh_Noun,Num_Noun,Gender_Noun), Num <> Num_Noun;
												
												partc_phrase( S_Start, S2,Padezh,Num,Gender),
												noun_phrase(S2,S_Stop,Padezh_Noun,Num_Noun,Gender_Noun), Gender <> Gender_Noun;
												
												partc_phrase( S_Start, S2,Padezh,Num,Gender),
												noun_phrase(S2,S_Stop,Padezh_Noun,Num_Noun,Gender_Noun), Padezh <> Padezh_Noun;
												
												partc_phrase(S_Start, S_Stop,Padezh,Num,Gender);
												 
												participle(S_Start, S_Stop, Padezh,Num,Gender);%, S_Stop =S_Start;
												
												
												mest_phrase(S_Start,S2,Padezh_Noun,Num_Noun,Gender_Noun),
												partc_phrase(S2,S_Stop,Padezh, Num,Gender), Num <> Num_Noun;
												
												mest_phrase(S_Start,S2,Padezh_Noun,Num_Noun,Gender_Noun),
												partc_phrase(S2,S_Stop,Padezh, Num,Gender), Gender <> Gender_Noun;
												
												mest_phrase(S_Start,S2,Padezh_Noun,Num_Noun,Gender_Noun),
												partc_phrase(S2,S_Stop,Padezh, Num,Gender), Padezh <> Padezh_Noun;
												
												partc_phrase( S_Start, S2,Padezh, Num,Gender),
												mest_phrase(S2,S_Stop,Padezh_Noun,Num_Noun,Gender_Noun), Num <> Num_Noun;

												partc_phrase( S_Start, S2,Padezh,Num,Gender),
												mest_phrase(S2,S_Stop,Padezh_Noun,Num_Noun,Gender_Noun), Gender <> Gender_Noun;
												
												partc_phrase( S_Start, S2,Padezh, Num,Gender),
												mest_phrase(S2,S_Stop,Padezh_Noun,Num_Noun,Gender_Noun), Padezh <> Padezh_Noun;
												
																								
												partc_phrase(S_Start, S_Stop,Padezh,Num,Gender);
												 
												participle(S_Start, S_Stop,Padezh, Num,Gender).%, S_Stop = S_Start.

	%√–”ѕѕџ
	%
	noun_group(S_Start, S_Stop) :-  
														noun_phrase(S_Start,[Head | [Head2 | Tail]],Padezh,Num,Gender),
														partc_ob([Head | [", " |[Head2 | Tail]]], S_Stop,Padezh,Num,Gender);
														
														partc_ob(S_Start, S2,Padezh,Num,Gender),
														noun_phrase(S2, S_Stop,Padezh,Num,Gender);
														
														noun_phrase(S_Start, S_Stop,_,_,_);
														
														mest_phrase(S_Start,S2,Padezh,Num,Gender),
														partc_ob(S2, S_Stop,Padezh, Num,Gender);
														
														partc_ob(S_Start, S2,Padezh, Num,Gender),
														mest_phrase(S2, S_Stop,Padezh,Num,Gender);
														
														mest_phrase(S_Start, S_Stop,_,_,_).
														
	%
	%verb_group(Main_Verb, Main_Noun, Main_Ger, Start, Stop) :- 	verb_phrase(_, _, Main_Verb, Start_Verb, Stop_Verb), 
	%														noun_phrase(_, _, _, _, Main_Noun, Start_Noun, Stop_Noun),
																												
	verb_group(S_Start, S_Stop) :- 	verb_phrase(S_Start, S2), noun_phrase(S2, S_Stop,_,_,_);
								noun_phrase(S_Start, S2,_,_,_), verb_phrase(S2, S_Stop);
								verb_phrase(S_Start, S_Stop).		
	
/*****************************************************************************

		—Ћќ¬ј–№
		
******************************************************************************/
	%ѕ–≈ƒЋќ√»
	pretext(["без"| X], X).
	pretext(["близ"| X], X).
	pretext(["благодар€"| X], X).
	
	pretext(["вопреки"| X], X).
	pretext(["в"| X], X).
	pretext(["во"| X], X).
	pretext(["вдоль"| X], X).
	pretext(["вроде"| X], X).
	pretext(["вследствие"| X], X).
	pretext(["возле"| X], X).
	pretext(["ввиду"| X], X).
	
	pretext(["дл€"| X], X).
	pretext(["до"| X], X).
	
	pretext(["за"| X], X).
	
	pretext(["из"| X], X).
	pretext(["из-за"| X], X).
	pretext(["из-под"| X], X).
	
	pretext(["к"| X], X).
	pretext(["ко"| X], X).
	pretext(["кроме"| X], X).
	
	pretext(["между"| X], X).
	
	pretext(["над"| X], X).
	pretext(["на"| X], X).
	pretext(["навстречу"| X], X).
	pretext(["наперекор"| X], X).
	
	pretext(["от"| X], X).
	pretext(["о"| X], X).
	pretext(["об"| X], X).
	pretext(["около"| X], X).
	
	pretext(["при"| X], X).
	pretext(["про"| X], X).
	pretext(["перед"| X], X).
	pretext(["после"| X], X).
	pretext(["позади"| X], X).
	pretext(["подобно"| X], X).
	pretext(["под"| X], X).
	pretext(["по"| X], X).
	
	pretext(["ради"| X], X).
	
	pretext(["с"| X], X).
	pretext(["со"| X], X).
	pretext(["сквозь"| X], X).
	pretext(["согласно"| X], X).
	
	pretext(["у"| X], X).
	
	
	pretext(["через"| X], X).
	
	
	%—ќё«џ
	union(["что"| X], X).
	union(["чтобы"| X], X).
	union(["чтоб"| X], X).
	union(["кто"| X], X).
	union(["когда"| X], X).
	union(["а"| X], X).
	union(["но"| X], X).
	union(["однако"| X], X).
	union(["зато"| X], X).
	union(["где"| X], X).
	union(["куда"| X], X).
	union(["откуда"| X], X).
	union(["будто"| X], X).
	union(["как"| X], X).
	union(["оттого"| X], X).
	union(["оттого-что"| X], X).
	union(["если"| X], X).
	union(["какой"| X], X).
	union(["кака€"| X], X).
	union(["какое"| X], X).
	union(["какие"| X], X).
	union(["какими"| X], X).
	union(["каким"| X], X).
	union(["каких"| X], X).
	union(["какого"| X], X).
	union(["какому"| X], X).
	union(["который"| X], X).
	union(["которые"| X], X).
	union(["котора€"| X], X).
	union(["которое"| X], X).
	union(["которому"| X], X).
	union(["которым"| X], X).
	union(["которыми"| X], X).
	union(["которых"| X], X).
	union(["которым"| X], X).
	union(["которого"| X], X).
	union(["сколько"| X], X).
	union(["почему"| X], X).
	union(["причем"| X], X).
	union(["словно"| X], X).
	union(["то"| X], X).
	union(["только"| X], X).
	union(["хот€"| X], X).
	union(["или"| X], X).
	union(["и"| X], X).
	
	union_unq(["или"| X], X).
	union_unq(["и"| X], X).
	union_unq(["да"| X], X).




	%ћ≈—“ќ»ћ≈Ќ»я
	mest(["заглушка"| X], X, 1,2,3).
	%—”ў≈—“¬»“≈Ћ№Ќџ≈
	noun(["Ѕыки"| X], X, 1,1,1).
	noun(["волки"| X], X, 1,1,1).
	noun(["врагами"| X], X, 5,1,1).
	noun(["друг друга"| X], X, 2,1,1).
	noun(["дождь"| X], X, 1,0,1).
	noun(["ночи"| X], X, 2,0,2).	
	%√Ћј√ќЋџ
	verb(["не люб€т"|X],X,2,3).
	verb(["были"| X], X, 2,3).
	verb(["был"| X], X, 2,3).
	verb(["уважают"| X], X,2,3).
	%ѕ–»Ћј√ј“≈Ћ№Ќџ≈
	adj(["’олодный"| X], X).
	%Ќј–≈„»я
	adv(["вcегда"| X], X).
	adv(["—егодн€"| X], X).
	%ѕ–»„ј—“»я
	participle(["шедший"| X], X,1,0,1).
	
	%ƒ≈≈ѕ–»„ј—“»я
	gerund(["заглушка"| X], X).
		
	
	
sentence(S1, S3) :- 	noun_group(S1, S2), verb_group(S2, S3);
						verb_group(S1, S2), noun_group(S2, S3).	

test_sentence(Str) :- split(Str, X), sentence(X, []).

find_partc([],[]) :-!.
find_partc([Head|Tail], Ans) :- partc_ob([Head | Tail], [],_,_,_), Ans = Tail.
find_partc([Head|Tail], Ans) :- find_partc(Tail, Ans).

test_partc(Str, Ans) :- split(Str,X), find_partc(X, Ans).

%solve(Str, RealAns) :- split(Str, X), comma_rule(X, Ans), my_concat(Ans, RealAns).

goal
	test_partc("’олодный дождь шедший с ночи был непри€тным", Ans).
	%test_sentence("’олодный дождь шедший с ночи был непри€тным").
	%solve("Ѕыки и волки всегда были врагами которые не люб€т друг друга но уважают", Ans).
	
