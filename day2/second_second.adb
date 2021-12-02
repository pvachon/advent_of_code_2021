with Ada.Text_IO,
     Ada.Command_Line,
     Ada.Strings,
     Ada.Strings.Fixed;

procedure Second_Second is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;
    package SF renames Ada.Strings.Fixed;

    file : Text_IO.File_Type;

    horiz : Natural := 0;
    depth : Natural := 0;
    aim : Natural := 0;

    procedure Handle_Command(raw_command : in String) is
        type Command is (Up, Down, Forward);
        cmd : Command := Up;
        value : Natural := 0;
        space : Natural := 0;

        function Parse_Command(prefix : in String) return Command is

        begin
            if prefix = "up" then
                return Up;
            elsif prefix = "forward" then
                return Forward;
            elsif prefix = "down" then
                return Down;
            else
                raise Ada.Strings.Index_Error;
            end if;
        end Parse_Command;
    begin
        space := SF.Index(Source => raw_command,
                          Pattern => " ");

        cmd := Parse_Command(raw_command(raw_command'First..space - 1));
        value := Natural'Value(raw_command(space + 1..raw_command'Last));

        -- Text_IO.Put_Line("Command: " & Command'Image(cmd) & " Value: " & Natural'Image(value));

        case cmd is
            when Up => aim := aim - value;
            when Down => aim := aim + value;
            when Forward =>
                horiz := horiz + value;
                depth := depth + (aim * value);
        end case;
    end Handle_Command;
begin

    Text_IO.Put_Line("Advent of Code 2021: 2.1");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify file of depths. Aborting.");
        return;
    end if;

	Text_IO.Put_Line("File name: " & Command_Line.Argument(1));

    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    while not Text_IO.End_Of_File(file) loop
        Handle_Command(Text_IO.Get_Line(file));

        Text_IO.Put_Line("Depth: " & Natural'Image(depth) & " Horizontal: " & Natural'Image(horiz) &
                         " Aim: " & Natural'Image(aim));
    end loop;

    Text_IO.Put_Line("Depth&Horiz Product: " & Integer'Image(depth * horiz));

end Second_Second;
