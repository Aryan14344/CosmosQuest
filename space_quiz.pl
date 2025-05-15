% space_quiz.pl
% Knowledge base for CosmoQuiz, a space-themed quiz website

% --- Facts about Space Entities ---

% Planets: name, notable_feature, type
planet(mercury, closest_to_sun, rocky).
planet(venus, hottest, rocky).
planet(earth, supports_life, rocky).
planet(mars, red_planet, rocky).
planet(jupiter, largest, gas_giant).
planet(saturn, rings, gas_giant).
planet(uranus, tilted_axis, ice_giant).
planet(neptune, windiest, ice_giant).

% Stars: name, type, notable_feature
star(sun, yellow_dwarf, '4.6_billion_years_old').
star(betelgeuse, red_supergiant, 'in_constellation_orion').
star(sirius, white_dwarf, 'brightest_in_sky').
star(polaris, yellow_supergiant, 'north_star').

% Cosmic Phenomena: name, property, description
phenomenon(black_hole, extreme_gravity, 'light_cannot_escape').
phenomenon(nebula, star_birthplace, 'gas_and_dust_cloud').
phenomenon(supernova, star_explosion, 'massive_energy_release').
phenomenon(galaxy, star_collection, 'contains_solar_systems').

% Fascinating Facts: entity, fact
fact(mercury, 'Mercury has no atmosphere, so its surface is scarred by meteor impacts.').
fact(jupiter, 'Jupiter’s Great Red Spot is a storm larger than Earth, raging for centuries.').
fact(black_hole, 'A black hole’s gravity is so strong that even light cannot escape it.').
fact(nebula, 'The Eagle Nebula contains the Pillars of Creation, where stars are born.').
fact(sun, 'The Sun accounts for 99.86% of the Solar System’s mass.').
fact(saturn, 'Saturn’s rings are made of ice and rock, some as big as mountains.').
fact(galaxy, 'The Milky Way is our galaxy, containing up to 400 billion stars.').
fact(polaris, 'Polaris, the North Star, is actually a triple star system.').

% --- Question Generation Rules ---

% Question about planet type
question_planet_type(Planet, Q, A) :-
    planet(Planet, _, Type),
    atom_concat('What type of planet is ', Planet, Q1),
    atom_concat(Q1, '?', Q),
    A = Type.

% Question about planet feature
question_planet_feature(Planet, Q, A) :-
    planet(Planet, Feature, _),
    atom_concat('Which planet is known as the ', Feature, Q1),
    atom_concat(Q1, '?', Q),
    A = Planet.

% Question about star type
question_star_type(Star, Q, A) :-
    star(Star, Type, _),
    atom_concat('What type of star is ', Star, Q1),
    atom_concat(Q1, '?', Q),
    A = Type.

% Question about phenomenon property
question_phenomenon_property(Phenomenon, Q, A) :-
    phenomenon(Phenomenon, Property, _),
    atom_concat('What cosmic phenomenon is known for its ', Property, Q1),
    atom_concat(Q1, '?', Q),
    A = Phenomenon.

% --- Multiple-Choice Question Generation ---

% Helper: Select N random elements from a list
random_select(Selected, List, N) :-
    length(List, Len),
    random_permutation(List, Perm),
    length(Selected, N),
    append(Selected, _, Perm).

% Multiple-choice question for planet type
mc_question_planet_type(Planet, Q, Correct, Wrongs) :-
    question_planet_type(Planet, Q, Correct),
    findall(T, (planet(_, _, T), T \= Correct), Types),
    random_select(Wrongs, Types, 3).

% Multiple-choice question for planet feature
mc_question_planet_feature(Planet, Q, Correct, Wrongs) :-
    question_planet_feature(Planet, Q, Correct),
    findall(P, (planet(P, _, _), P \= Correct), Planets),
    random_select(Wrongs, Planets, 3).

% Multiple-choice question for star type
mc_question_star_type(Star, Q, Correct, Wrongs) :-
    question_star_type(Star, Q, Correct),
    findall(T, (star(_, T, _), T \= Correct), Types),
    random_select(Wrongs, Types, 3).

% Multiple-choice question for phenomenon property
mc_question_phenomenon_property(Phenomenon, Q, Correct, Wrongs) :-
    question_phenomenon_property(Phenomenon, Q, Correct),
    findall(P, (phenomenon(P, _, _), P \= Correct), Phenomena),
    random_select(Wrongs, Phenomena, 3).

% Combine all question types
mc_question(Q, Correct, Wrongs) :-
    ( mc_question_planet_type(_, Q, Correct, Wrongs)
    ; mc_question_planet_feature(_, Q, Correct, Wrongs)
    ; mc_question_star_type(_, Q, Correct, Wrongs)
    ; mc_question_phenomenon_property(_, Q, Correct, Wrongs)
    ).

% Generate 10 unique quiz questions
quiz_questions(Questions, 10) :-
    findall(q(Q, Correct, Wrongs), mc_question(Q, Correct, Wrongs), AllQs),
    random_select(Questions, AllQs, 10).

% --- Fact Retrieval ---

% Select a random fact
random_fact(Fact) :-
    findall(F, fact(_, F), Facts),
    random_member(Fact, Facts).