with Ada.Text_IO,
     Ada.Command_Line;

procedure One_Second is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;

    file : Text_IO.File_Type;

    cur_sum : Integer := 0;
    prev_sum : Integer := 0;
    greater : Integer := 0;
    value : Integer := 0;

    type Window_Loc is new Integer range 1..3;
    type Window is array(Window_Loc) of Integer;

    win : Window;
    loc : Window_Loc := Window_Loc'First;
begin

    Text_IO.Put_Line("Advent of Code 2021: 1.1");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify file of depths. Aborting.");
        return;
    end if;

	Text_IO.Put_Line("File name: " & Command_Line.Argument(1));

    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    win(1) := Integer'Value(Text_IO.Get_Line(file));
    win(2) := Integer'Value(Text_IO.Get_Line(file));
    win(3) := Integer'Value(Text_IO.Get_Line(file));
    prev_sum := win(1) + win(2) + win(3);
    while not Text_IO.End_Of_File(file) loop
        value := Integer'Value(Text_IO.Get_Line(file));
        cur_sum := prev_sum - win(loc) + value;
        win(loc) := value;

        if loc = Window_Loc'Last then
            loc := Window_Loc'First;
        else
            loc := loc + 1;
        end if;

        if cur_sum > prev_sum then
            greater := greater + 1;
        end if;

        prev_sum := cur_sum;
    end loop;

    Text_IO.Put_Line("Number greater: " & Integer'Image(greater));

end One_Second;
