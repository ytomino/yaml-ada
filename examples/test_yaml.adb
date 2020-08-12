with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with YAML.Streams;
procedure test_yaml is
	function "=" (Left, Right : YAML.Event) return Boolean is
		use YAML;
	begin
		if Left.Event_Type /= Right.Event_Type then
			return False;
		else
			case Left.Event_Type is
				when Stream_Start =>
					return Left.Encoding = Right.Encoding;
				when Document_Start | Document_End =>
					if Left.Implicit_Indicator /= Right.Implicit_Indicator then
						return False;
					end if;
					case Left.Event_Type is
						when Document_Start =>
							if Left.Version_Directive = null then
								if Right.Version_Directive /= null then
									return False;
								end if;
							elsif Right.Version_Directive = null then
								return False;
							elsif Left.Version_Directive.all /= Right.Version_Directive.all then
								return False;
							end if;
							if Left.Tag_Directives = null then
								return Right.Tag_Directives = null;
							elsif Right.Tag_Directives = null then
								return False;
							else
								raise Program_Error; -- unimplemented
							end if;
						when others =>
							return True;
					end case;
				when Alias | Scalar | Sequence_Start | Mapping_Start =>
					if Left.Anchor = null then
						if Right.Anchor /= null then
							return False;
						end if;
					elsif Right.Anchor = null then
						return False;
					elsif Left.Anchor.all /= Right.Anchor.all then
						return False;
					end if;
					case Left.Event_Type is
						when Scalar | Sequence_Start | Mapping_Start =>
							if Left.Tag = null then
								if Right.Tag /= null then
									return False;
								end if;
							elsif Right.Tag = null then
								return False;
							elsif Left.Tag.all /= Right.Tag.all then
								return False;
							end if;
							case Left.Event_Type is
								when Scalar =>
									if Left.Value.all /= Right.Value.all then
										return False;
									end if;
									return Left.Plain_Implicit_Tag = Right.Plain_Implicit_Tag
										and then Left.Quoted_Implicit_Tag = Right.Quoted_Implicit_Tag
										and then Left.Scalar_Style = Right.Scalar_Style;
								when Sequence_Start | Mapping_Start =>
									if Left.Implicit_Tag /= Right.Implicit_Tag then
										return False;
									end if;
									case Left.Event_Type is
										when Sequence_Start =>
											return Left.Sequence_Style = Right.Sequence_Style;
										when Mapping_Start =>
											return Left.Mapping_Style = Right.Mapping_Style;
										when others =>
											return True;
									end case;
								when others =>
									return True;
							end case;
						when others =>
							return True;
					end case;
				when No_Event | Stream_End | Sequence_End | Mapping_End =>
					return True;
			end case;
		end if;
	end "=";
	Test_File_Name : constant String := "test_yaml.yaml";
	type Event_Constant is access constant YAML.Event;
	The_Tag_1 : aliased constant String := "!TEST";
	The_Value_1 : aliased constant String := "Hello";
	The_Value_2 : aliased constant String := "YAML";
	Data : constant array (Positive range <>) of not null Event_Constant := (
		new YAML.Event'(
			Event_Type => YAML.Stream_Start,
			Encoding => YAML.UTF_8),
		new YAML.Event'(
			Event_Type => YAML.Document_Start,
			Implicit_Indicator => False,
			Version_Directive => null,
			Tag_Directives => null),
		new YAML.Event'(
			Event_Type => YAML.Mapping_Start,
			Anchor => null,
			Tag => The_Tag_1'Unchecked_Access,
			Implicit_Tag => False,
			Mapping_Style => YAML.Block),
		new YAML.Event'(
			Event_Type => YAML.Scalar,
			Anchor => null,
			Tag => null,
			Value => The_Value_1'Unchecked_Access,
			Plain_Implicit_Tag => True,
			Quoted_Implicit_Tag => False,
			Scalar_Style => YAML.Plain),
		new YAML.Event'(
			Event_Type => YAML.Scalar,
			Anchor => null,
			Tag => null,
			Value => The_Value_2'Unchecked_Access,
			Plain_Implicit_Tag => True,
			Quoted_Implicit_Tag => False,
			Scalar_Style => YAML.Plain),
		new YAML.Event'(
			Event_Type => YAML.Mapping_End),
		new YAML.Event'(
			Event_Type => YAML.Document_End,
			Implicit_Indicator => True),
		new YAML.Event'(
			Event_Type => YAML.Stream_End));
begin
	declare
		W : YAML.Emitter := YAML.Create (Ada.Text_IO.Put'Access);
	begin
		for I in Data'Range loop
			YAML.Emit (W, Data (I).all);
		end loop;
		YAML.Flush (W);
	end;
	declare
		File : Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Create (
			File,
			Name => Test_File_Name);
		declare
			W : YAML.Emitter := YAML.Streams.Create (
				Ada.Streams.Stream_IO.Stream (File));
		begin
			Ada.Text_IO.Put ("Writing...");
			for I in Data'Range loop
				Ada.Text_IO.Put (I'Img);
				YAML.Emit (W, Data (I).all);
			end loop;
			YAML.Flush (W);
			Ada.Text_IO.Put_Line (" ok");
		end;
		Ada.Streams.Stream_IO.Close (File);
	end;
	declare
		File : Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Open (
			File,
			Ada.Streams.Stream_IO.In_File,
			Name => Test_File_Name);
		declare
			R : YAML.Parser := YAML.Streams.Create (
				Ada.Streams.Stream_IO.Stream (File));
		begin
			Ada.Text_IO.Put ("Reading...");
			for I in Data'Range loop
				Ada.Text_IO.Put (I'Img);
				declare
					procedure Process (
						Event : in YAML.Event;
						Start_Mark, End_Mark : in YAML.Mark)
					is
						pragma Unreferenced (Start_Mark);
						pragma Unreferenced (End_Mark);
					begin
						if Event /= Data (I).all then
							raise Program_Error;
						end if;
					end Process;
				begin
					YAML.Parse (R, Process'Access);
				end;
			end loop;
			Ada.Text_IO.Put_Line (" ok");
		end;
		Ada.Streams.Stream_IO.Close (File);
	end;
end test_yaml;
