:- use_module(library(random)).


space_fact("The Sun accounts for 99.86% of the mass in our solar system.").
space_fact("A day on Venus is longer than its year. Venus rotates very slowly.").
space_fact("Olympus Mons on Mars is the largest volcano in the solar system, about 2.5 times Mount Everest's height.").
space_fact("There are more stars in the universe than grains of sand on all the beaches on Earth.").
space_fact("The Milky Way galaxy is estimated to contain 100 to 400 billion stars.").
space_fact("If you could drive a car upwards at 60 mph (95 km/h), it would take about an hour to reach outer space.").
space_fact("Neutron stars are so dense that a teaspoonful would weigh billions of tons on Earth.").
space_fact("The closest star system to us is Alpha Centauri, about 4.37 light-years away.").
space_fact("Footprints and tire tracks left by astronauts on the Moon will stay there forever as there is no wind to blow them away.").
space_fact("Saturn's rings are made mostly of ice particles with a smaller amount of rocky debris and dust.").
space_fact("Jupiter's Great Red Spot is a giant storm larger than Earth that has raged for hundreds of years.").
space_fact("Light from the Sun takes about 8 minutes and 20 seconds to reach Earth.").
space_fact("Space is completely silent because sound waves need a medium to travel through.").
space_fact("The Andromeda Galaxy is the closest large spiral galaxy to the Milky Way and is expected to collide with us in about 4.5 billion years.").
space_fact("A black hole is a region of spacetime where gravity is so strong that nothing, not even light, can escape.").

% Predicate to get a random fact
get_random_fact(Fact) :-
    findall(F, space_fact(F), FactsList), % Collect all facts into a list
    random_member(Fact, FactsList).      % Pick one fact randomly from the list

% Command-line goal:
% To run from command line and get a fact:
% swipl -s facts.pl -g "get_random_fact(Fact), writeln(Fact), halt."