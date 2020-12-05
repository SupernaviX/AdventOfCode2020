"Seating Shenanigans" by Simon Gellis

[Without this it crashes after 100 entries in the table]
Use MAX_STATIC_DATA of 3600000;

The plane is a room. The printed name of the plane is "Plane (Entrance)".

Table 1 - Boarding Passes
Seat ID (a number)
with 1024 blank rows

The highest seat ID is a number that varies. The highest seat ID is initially 0.
The lowest seat ID is a number that varies. The lowest seat ID is initially 1024.

When play begins:
	say "You are boarding a plane, only to discover a new problem; you dropped your boarding pass! You aren't sure which seat is yours, and all of the flight attendants are busy with the flood of people that suddenly made it through passport control.[paragraph break]";
	say "Following the grand tradition of adventure games, you ignore several obvious solutions in favor of something complicated and fun. You write a quick program to use your phone's camera to scan all of the nearby boarding passes (your puzzle input); perhaps you can find your seat through process of elimination.[paragraph break]";
	say "In just a few moments, you've used your phone's touchscreen to write highly-effective OCR software. With that done, you loiter at the entrance to the plane, 'subtly' scanning boarding passes as people come in.[paragraph break]";
	now the command prompt is "What's the next pass? >".
	
To decide whether scanning passes:
	if the command prompt is "What's the next pass? >", yes;
	no.

After reading a command when scanning passes:
	let the current pass be the player's command;
	let the current seat ID be the seat ID of the current pass;
	choose a blank row in the Table of Boarding Passes;
	now the seat ID entry is the current seat ID;
	say "[one of]You scan[or]You catch a glimpse of[or]Ignoring odd looks, you scan[as decreasingly likely outcomes] [current pass], seat #[current seat ID].";
	if the current seat ID is greater than the highest seat ID:
		now the highest seat ID is the current seat ID;
	if the current seat ID is less than the lowest seat ID:
		now the lowest seat ID is the current seat ID;
	reject the player's command.

To decide which number is the seat ID of (pass - a text):
	if the number of characters in pass is not 10:
		end the story saying "You read a pass wrong...";
	let row be zero;
	let letter-value be 128;
	repeat with char-number running from one to seven:
		let letter be character number char-number in pass;
		let letter-value be letter-value divided by two; 
		if letter is "b":
			let row be row plus letter-value;
	let column be zero;
	let letter-value be eight;
	repeat with char-number running from eight to ten:
		let letter be character number char-number in pass;
		let letter-value be letter-value divided by two;
		if letter is "r":
			let column be column plus letter-value;
	decide on row times eight plus column.
	
To decide which number is your seat:
	repeat with seat ID running from the lowest seat ID to the highest seat ID:
		if the seat ID is not a seat ID listed in the Table of Boarding Passes:
			decide on the seat ID.
	
[The input file ends with a blank line, which triggers a parser error. When we get that error, finish processing input.]
Rule for printing a parser error when scanning passes:
	say "You read all the passes! Good work!";
	say "The highest seat ID is [italic type]#[highest seat ID][roman type].";
	say "Thinking quickly, you search for any pass numbers which you DIDN'T scan.";
	say "Through process of elimination, your seat must be [italic type]#[your seat][roman type]!";
	say "You quickly sit down and relax.";
	now the printed name of the plane is "Plane (Seat #[your seat])";
	now the command prompt is ">";
