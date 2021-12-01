with Ada.Text_IO,
     Ada.Command_Line;

procedure One_First is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;

    file : Text_IO.File_Type;

    value : Integer := 0;
    prev_value : Integer := 0;
    greater : Integer := 0;
begin

    Text_IO.Put_Line("Advent of Code 2021: 1.0");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify file of depths. Aborting.");
        return;
    end if;

	Text_IO.Put_Line("File name: " & Command_Line.Argument(1));

    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    prev_value := Integer'Value(Text_IO.Get_Line(file));
    while not Text_IO.End_Of_File(file) loop
        value := Integer'Value(Text_IO.Get_Line(file));

        if value > prev_value then
            greater := greater + 1;
        end if;

        prev_value := value;
    end loop;

    Text_IO.Put_Line("Number greater: " & Integer'Image(greater));

end One_First;
