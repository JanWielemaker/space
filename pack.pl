name(space).
version('0.3.5').
title('Space package').
keywords(['geo', 'spatial']).
author( 'Willem van Hage', 'w.vanhage@esciencecenter.nl' ).
packager( 'Jan Wielemaker', 'J.Wielemaker@vu.nl' ).
maintainer( 'Jan Wielemaker', 'J.Wielemaker@vu.nl' ).
home( 'https://github.com/JanWielemaker/space.git' ).
download( 'https://github.com/JanWielemaker/space/archive/V*.zip' ).
pack_version(2).
% requires(prolog >= "9.3.8"). % TODO: stable version
requires(prolog:c_cxx(_)).

