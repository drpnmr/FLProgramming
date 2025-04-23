% genre(Жанр, Вымышленность, Фантастика, Страх, Любовь, Детектив, Реализм)
genre(science_fiction,     1, 1, 0, 0, 0, 0).
genre(fantasy,             1, 1, 0, 0, 0, 0).
genre(dystopian,           1, 1, 0, 0, 0, 1).
genre(cyberpunk,           1, 1, 1, 0, 0, 0).
genre(post_apocalyptic,    1, 1, 1, 0, 0, 0).
genre(detective,           0, 0, 1, 0, 1, 0).
genre(thriller,            0, 0, 1, 0, 0, 0).
genre(horror,              0, 1, 1, 0, 0, 0).
genre(romance,             0, 0, 0, 1, 0, 0).
genre(historical_romance,  0, 0, 0, 1, 0, 1).
genre(drama,               0, 0, 0, 1, 0, 1).
genre(historical_fiction,  0, 0, 0, 0, 0, 1).
genre(biography,           0, 0, 0, 0, 0, 1).
genre(memoir,              0, 0, 0, 0, 0, 1).
genre(crime_novel,         0, 0, 1, 0, 1, 0).
genre(police_procedural,   0, 0, 1, 0, 1, 0).
genre(romantic_thriller,   0, 0, 1, 1, 0, 0).
genre(fantasy_romance,     1, 1, 0, 1, 0, 0).
genre(science_romance,     1, 1, 0, 1, 0, 0).
genre(urban_fantasy,       1, 1, 1, 0, 0, 0).
genre(magic_realism,       0, 1, 0, 1, 0, 1).
genre(paranormal_romance,  1, 1, 1, 1, 0, 0).
genre(techno_thriller,     1, 1, 1, 0, 1, 0).
genre(science_mystery,     1, 1, 0, 0, 1, 0).
genre(suspense,            0, 0, 1, 0, 1, 0).
genre(legal_thriller,      0, 0, 1, 0, 1, 1).
genre(historical_mystery,  0, 0, 1, 0, 1, 1).
genre(war_novel,           0, 0, 1, 0, 0, 1).
genre(epic_fantasy,        1, 1, 0, 0, 0, 0).
genre(family_saga,         0, 0, 0, 1, 0, 1).
genre(literary_fiction,    0, 0, 0, 0, 0, 1).

question1(X1):- 
    write("Does the story take place in a future or fictional world? (1 - yes, 0 - no)"), nl,
    read(X1).

question2(X2):- 
    write("Are there elements of sci-fi or magic? (1 - yes, 0 - no)"), nl,
    read(X2).

question3(X3):- 
    write("Is it a tense or scary story? (1 - yes, 0 - no)"), nl,
    read(X3).

question4(X4):- 
    write("Is it about love or relationships? (1 - yes, 0 - no)"), nl,
    read(X4).

question5(X5):- 
    write("Is there an investigation or mystery? (1 - yes, 0 - no)"), nl,
    read(X5).

question6(X6):- 
    write("Is it based on real events or historical context? (1 - yes, 0 - no)"), nl,
    read(X6).

pr :-
    question1(X1), question2(X2), question3(X3),
    question4(X4), question5(X5), question6(X6),
    genre(Genre, X1, X2, X3, X4, X5, X6),
    write("Your genre is likely: "), write(Genre), nl,
    fail.  % To show all possible matches
pr :- 
    write("No matching genre found."), nl.