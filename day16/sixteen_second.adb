with Ada.Text_IO,
     Ada.Command_Line,
     Ada.Strings,
     Ada.Containers.Vectors;

procedure Sixteen_Second is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;

    file : Text_IO.File_Type;

    type Nibble is mod 2**4;
    type Field is mod 2**64;

    type Nibble_Vector is array(Natural range <>) of Nibble
        with Default_Component_Value => 0;

    type Biterator(Length : Natural) is record
        vector : Nibble_Vector(1..Length);
        bit_pos : Natural := 0;
    end record;

    function Shift_Left(v : in Nibble; l : in Natural) return Nibble is begin
        return v * 2**l;
    end Shift_Left;

    function Shift_Left(v : in Field; l : in Natural) return Field is begin
        return v * 2**l;
    end Shift_Left;


    procedure Biterator_Advance(b : in out Biterator;
                                amount : in Natural) is
    begin
        b.bit_pos := b.bit_pos + amount;
    end Biterator_Advance;

    function Biterator_Remaining(b : in Biterator) return Natural is
    begin
        return (b.vector'Length * 4) - b.bit_pos;
    end Biterator_Remaining;

    function Biterator_Consumed(b : in Biterator) return Natural is begin
        return b.bit_pos;
    end Biterator_Consumed;

    function Biterator_Extract(b            : in out Biterator;
                               field_len    : in Natural) return Field
    is
        accum       : Field := 0;
        mask        : Nibble := 0;
        nibble_pos  : Natural := 0;
    begin
        for I in 1..field_len loop
            nibble_pos := (b.bit_pos / 4) + 1;
            mask := Shift_Left(1, 3 - (b.bit_pos mod 4));
            accum := (accum * 2) or (if (b.vector(nibble_pos) and mask) /= 0 then 1 else 0);
            Biterator_Advance(b, 1);
        end loop;

        return accum;
    end Biterator_Extract;

    function From_Hex(c : Character) return Nibble is begin
        case c is
            when 'A'..'F' => return Nibble(Character'Pos(c) - Character'Pos('A') + 10);
            when '0'..'9' => return Nibble(Character'Pos(c) - Character'Pos('0'));
            when others => raise Ada.Strings.Index_Error;
        end case;
    end From_Hex;

    function Decode_Literal_Field(iter      : in out Biterator;
                                  length    : out Natural) return Field
    is
        pk_arm_type : Field := 0;
        pk_literal : Field := 0;
    begin
        length := 0;

        loop
            pk_arm_type := Biterator_Extract(iter, 1);

            pk_literal := Shift_Left(pk_literal, 4) or Biterator_Extract(iter, 4);

            length := length + 5;

            exit when pk_arm_type = 0;
        end loop;

        if length >= 80 then
            Text_IO.Put_Line("Overflow. Length is " & Natural'Image(length));
            raise Ada.Strings.Index_Error;
        end if;

        return pk_literal;
    end;

    type Count_Type is (Packet_Count, Bit_Count);

    type Operation is (Finish, Sum, Product, Minimum, Maximum, Greater_Than, Less_Than, Equal_To);
    type Packet_Context is record
        op : Operation;
        count_kind : Count_Type;
        count : Natural;
        processed_pkts : Natural := 0;
        processed_bits : Natural := 0;
    end record;

    package Packet_Context_Stack is new Ada.Containers.Vectors(
        Element_Type => Packet_Context,
        Index_Type => Natural);
    context_stack : Packet_Context_Stack.Vector;

    package Operand_Stack is new Ada.Containers.Vectors(
        Element_Type => Field,
        Index_Type => Natural);
    op_stack : Operand_Stack.Vector;

    procedure Push(f : Field) is begin
        op_stack.Append(f);
    end Push;

    function Pop return Field is
        f : Field := op_stack(op_stack.Last_Index);
    begin
        op_stack.Delete_Last(Count => 1);
        return f;
    end Pop;

    procedure Push(ctx : Packet_Context) is begin
        --Text_IO.Put_Line("Push: " & Packet_Context'Image(ctx));
        context_stack.Append(ctx);
    end Push;

    function Pop return Packet_Context is
        c : Packet_Context := context_stack(context_stack.Last_Index);
    begin
        context_stack.Delete_Last(Count => 1);
        --Text_IO.Put_Line("Pop: " & Packet_Context'Image(c));
        return c;
    end Pop;

    function Is_Empty_Stack return Boolean is begin
        return context_stack.Is_Empty;
    end Is_Empty_Stack;

    procedure Read_Packets(raw : String) is
        iter : Biterator(Length => raw'Length);
        pk_version, pk_type, pk_len_type : Field := 0;
        pk_len : Field := 0;

        literal : Field := 0;
        ver_sum : Field := 0;
        header_bits : Natural := 6;

        op : Operation := Finish;
        count_kind : Count_Type := Packet_Count;
        count : Natural := 1;
        processed_pkts : Natural := 0;
        processed_bits : Natural := 0;

        procedure Do_Operation is
        begin
            --Text_IO.Put_Line("Operation: " & Operation'Image(op) & " items: " & Natural'Image(processed_pkts));
            case op is
                when Finish => Do_Finish : declare
                        f : Field := Pop;
                    begin
                        Text_IO.Put_Line("Final value: " & Field'Image(f));
                    end Do_Finish;
                when Sum => Do_Sum : declare
                        f : Field := 0;
                        r : Field;
                    begin
                        for I in 1..processed_pkts loop
                            r := Pop;
                            f := f + r;
                        end loop;
                        Push(f);
                    end Do_Sum;
                when Product => Do_Product : declare
                            r, f : Field := 1;
                    begin
                        for I in 1..processed_pkts loop
                            r := Pop;
                            f := f * r;
                        end loop;
                        Push(f);
                    end Do_Product;
                when Minimum => Do_Minimum : declare
                        min : Field := Field'Last;
                        r : Field := 0;
                    begin
                        for I in 1..processed_pkts loop
                            r := Pop;
                            min := (if min > r then r else min);
                        end loop;
                        Push(min);
                    end Do_Minimum;
                when Maximum => Do_Maximum : declare
                        max : Field := Field'First;
                        r : Field := 0;
                    begin
                        for I in 1..processed_pkts loop
                            r := Pop;
                            max := (if max < r then r else max);
                        end loop;
                        Push(max);
                    end Do_Maximum;
                when Greater_Than => Do_Greater_Than : declare
                        op2 : Field := Pop;
                        op1 : Field := Pop;
                    begin
                        if processed_pkts /= 2 then
                            raise Ada.Strings.Index_Error;
                        end if;
                        if op1 > op2 then
                            Push(1);
                        else
                            Push(0);
                        end if;
                    end Do_Greater_Than;
                when Less_Than => Do_Less_Than : declare
                        op2 : Field := Pop;
                        op1 : Field := Pop;
                    begin
                        if processed_pkts /= 2 then
                            raise Ada.Strings.Index_Error;
                        end if;
                        if op1 < op2 then
                            Push(1);
                        else
                            Push(0);
                        end if;
                    end Do_Less_Than;
                when Equal_To => Do_Equal_To : declare
                        op1 : Field := Pop;
                        op2 : Field := Pop;
                    begin
                        if processed_pkts /= 2 then
                            raise Ada.Strings.Index_Error;
                        end if;
                        if op1 = op2 then
                            Push(1);
                        else
                            Push(0);
                        end if;
                    end Do_Equal_to;
            end case;
        end Do_Operation;

        function Update_Context(nr_bits : in Natural) return Boolean is begin
            processed_pkts := processed_pkts + 1;
            processed_bits := processed_bits + nr_bits;

            case count_kind is
                when Bit_Count => return processed_bits = count;
                when Packet_Count => return processed_pkts = count;
            end case;
        end Update_Context;

        procedure Next_Packet is
            pk_version : Field := Biterator_Extract(iter, 3);
            pk_type : Field := Biterator_Extract(iter, 3);
            new_count : Field;
            new_count_kind : Count_Type;
            done : Boolean := False;
            pk_c_type : Field;
            new_op : Operation;
        begin
            ver_sum := ver_sum + pk_version;
            processed_bits := processed_bits + 6;

            if pk_type /= 4 then
                -- Figure out the kind of packet extract field data
                pk_c_type := Biterator_Extract(iter, 1);
                if pk_c_type = 1 then
                    new_count := Biterator_Extract(iter, 11);
                    new_count_kind := Packet_Count;
                    processed_bits := processed_bits + 12;
                else
                    new_count := Biterator_Extract(iter, 15);
                    new_count_kind := Bit_Count;
                    processed_bits := processed_bits + 16;
                end if;

                new_op := (case pk_type is
                    when 0 => Sum,
                    when 1 => Product,
                    when 2 => Minimum,
                    when 3 => Maximum,
                    when 5 => Greater_Than,
                    when 6 => Less_Than,
                    when 7 => Equal_To,
                    when others => raise Ada.Strings.Index_Error);

                -- Push the existing context; include the header bits
                Push((op => op,
                    count_kind => count_kind,
                    count => count,
                    processed_pkts => processed_pkts,
                    processed_bits => processed_bits));

                -- Record the current counts
                op := new_op;
                count := Natural(new_count);
                count_kind := new_count_kind;
                processed_pkts := 0;
                processed_bits := 0;
            else
                Eat_Literal : declare
                    field_len : Natural := 0;
                    literal : Field := Decode_Literal_Field(iter, field_len);
                    done : Boolean := false;
                begin
                    -- Push on to operand stack
                    Push(literal);
                    done := Update_Context(field_len);
                    Pop_Context : declare
                        c : Packet_Context;
                        bit_count : Natural;
                    begin
                        while done loop
                            -- Complete the open operation
                            Do_Operation;
                            exit when Is_Empty_Stack;

                            -- Pop the next operation
                            c := Pop;
                            bit_count := processed_bits;
                            op := c.op;
                            count_kind := c.count_kind;
                            count := c.count;
                            processed_pkts := c.processed_pkts;
                            processed_bits := c.processed_bits;
                            done := Update_Context(bit_count);
                        end loop;
                    end Pop_Context;
                end Eat_Literal;

            end if;
        end Next_Packet;
    begin
        -- Decode the raw bit vector containing the packets
        for J in raw'Range loop
            iter.vector(J) := From_Hex(raw(J));
        end loop;

        loop
            Next_Packet;
            exit when Is_Empty_Stack;
        end loop;
    end Read_Packets;
begin

    Text_IO.Put_Line("Advent of Code 2021: 16.1");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify problem file. Aborting.");
        return;
    end if;

    Text_IO.Put_Line("File name: " & Command_Line.Argument(1));
    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    Read_Packets(Text_IO.Get_Line(file));

end Sixteen_Second;
