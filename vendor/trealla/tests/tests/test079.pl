main :-
	N1 is -123456789012345678901234567890,
	N1 =:= -123456789012345678901234567890,
	write(N1), nl,
	N2 is float(1881676372353657772546715999894626455109783106026821047606410765129148590562263),
	write(N2), nl,
	N3 is gcd(987654321098765432109876543210, 42),
	N3 =:= 42,
	write(N3), nl,
	N4 is gcd(42, 987654321098765432109876543210),
	N4 =:= 42,
	write(N4), nl,
	N5 is gcd(987654321098765432109876543210, 123456789012345678901234567890),
	N5 =:= 9000000000900000000090,
	write(N5), nl,
	N6 is 15241578753238836750495351562536198787501905199875019052100 mod 1234567890123456789123,
	N6 =:= 122443787781019052100,
	write(N6), nl,
	N7 is 15241578753238836750495351562536198787501905199875019052100 rem 1234567890123456789123,
	N7 =:= 122443787781019052100,
	write(N7), nl,
	N8 is 15241578753238836750495351562536198787501905199875019052100 div 12345678901234567891234567,
	N8 =:= 1234567890123456788901234657800000,
	write(N8), nl,
	N9 is 370370367037037036703703703670 / 123456789012345678901234567890,
	N9 < (3.0 + 0.0000000001),
	N9 > (3.0 - 0.0000000001),
	write(N9), nl.

:- initialization(main).