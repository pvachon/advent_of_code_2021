with Ada.Text_IO,
     Ada.Command_Line;

procedure Six_First is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;

    file : Text_IO.File_Type;

    type Life_Cycle is range 0..8;

    type States is array(Life_Cycle) of Natural;
    fish_states : States := (others => 0);

    procedure Load_Initial_States(line : String) is
        pos : Natural := 1;
        state : Life_Cycle;
    begin
        while pos <= line'Length loop
            state := Character'Pos(line(pos)) - 48;
            fish_states(state) := fish_states(state) + 1;
            pos := pos + 2;
        end loop;
    end Load_Initial_States;

    procedure Dump_States is
        sum : Natural := 0;
    begin
        for I in States'Range loop
            Text_IO.Put(Life_Cycle'Image(I) & ": " & Natural'Image(fish_states(I)) & ", ");
            sum := sum + fish_states(I);
        end loop;
        Text_IO.Put_Line(" --> SUM: " & Natural'Image(sum));
    end Dump_States;
begin

    Text_IO.Put_Line("Advent of Code 2021: 6.0");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify initial problem. Aborting.");
        return;
    end if;

    Text_IO.Put_Line("File name: " & Command_Line.Argument(1));

    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    Load_Initial_States(Text_IO.Get_Line(file));

    Dump_States;

    for I in 1..80 loop
        declare
            new_states : States := (others => 0);
        begin
            -- If the fish are in state 0, copy them to state 8
            new_states(8) := fish_states(0);

            -- Now shift the array down
            new_states(7) := fish_states(8);
            -- Move the fish that just gave birth back to state 6
            new_states(6) := fish_states(7) + fish_states(0);
            new_states(5) := fish_states(6);
            new_states(4) := fish_states(5);
            new_states(3) := fish_states(4);
            new_states(2) := fish_states(3);
            new_states(1) := fish_states(2);
            new_states(0) := fish_states(1);

            fish_states := new_states;

            Dump_States;
        end;
    end loop;

end Six_First;
