package body Serialization is
	use type Ada.Strings.Unbounded.Unbounded_String;
	
	function Error_Message (Name, Value : String) return String is
	begin
		if Name = "" then
			return Value;
		else
			return Name & ": " & Value;
		end if;
	end Error_Message;
	
	-- private implementation
	
	procedure Advance_Structure (
		Object : not null access Reader;
		Position : in State)
	is
		type Class is not null access all Reader'Class;
	begin
		case Next_Kind (Class (Object)) is
			when Enter_Mapping =>
				Advance (Class (Object), Position);
				while Next_Kind (Class (Object)) /= Leave_Mapping loop
					Advance_Structure (Object, In_Mapping);
				end loop;
			when Enter_Sequence =>
				Advance (Class (Object), Position);
				while Next_Kind (Class (Object)) /= Leave_Sequence loop
					Advance_Structure (Object, In_Sequence);
				end loop;
			when others =>
				Advance (Class (Object), Position);
		end case;
	end Advance_Structure;
	
	-- implementation of scalar
	
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Value : in out Ada.Strings.Unbounded.Unbounded_String) is
	begin
		case Object.Direction is
			when Reading =>
				if Name = Next_Name (Object.Reader).all then
					Ada.Strings.Unbounded.Set_Unbounded_String (
						Value,
						Next_Value (Object.Reader).all);
				end if;
			when Writing =>
				Put (Object.Writer, Name, Ada.Strings.Unbounded.To_String (Value));
		end case;
	end IO;
	
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Value : in out Ada.Strings.Unbounded.Unbounded_String;
		Default : in Ada.Strings.Unbounded.Unbounded_String) is
	begin
		if Object.Direction = Reading or else Value /= Default then
			IO (Object, Name, Value);
		end if;
	end IO;
	
	procedure IO (
		Object : not null access Serializer;
		Value : in out Ada.Strings.Unbounded.Unbounded_String) is
	begin
		IO (Object, "", Value);
	end IO;
	
	procedure IO (
		Object : not null access Serializer;
		Value : in out Ada.Strings.Unbounded.Unbounded_String;
		Default : in Ada.Strings.Unbounded.Unbounded_String) is
	begin
		IO (Object, "", Value, Default);
	end IO;
	
	package body IO_Custom is
	
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Custom_Type) is
		begin
			case Object.Direction is
				when Reading =>
					if Name = Next_Name (Object.Reader).all then
						declare
							Next_Value : String
								renames Serialization.Next_Value (Object.Reader).all;
						begin
							Value := IO_Custom.Value (Next_Value);
						exception
							when Constraint_Error =>
								raise Constraint_Error with Error_Message (Name, Next_Value);
						end;
					end if;
				when Writing =>
					declare
						Im : String renames Image (Value);
						First : Positive := Im'First;
					begin
						if Triming and then First <= Im'Last and then Im (First) = ' ' then
							First := First + 1;
						end if;
						Put (Object.Writer, Name, Im (First .. Im'Last));
					end;
			end case;
		end IO;
		
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Custom_Type;
			Default : in Custom_Type) is
		begin
			if Object.Direction = Reading or else Value /= Default then
				IO (Object, Name, Value);
			end if;
		end IO;
		
	end IO_Custom;
	
	package IO_Integer is
		new IO_Custom (Integer, Integer'Image, Integer'Value, Triming => True);
	
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Value : in out Integer)
		renames IO_Integer.IO;
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Value : in out Integer;
		Default : in Integer)
		renames IO_Integer.IO;
	
	package IO_Boolean is
		new IO_Custom (Boolean, Boolean'Image, Boolean'Value, Triming => False);
	
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Value : in out Boolean)
		renames IO_Boolean.IO;
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Value : in out Boolean;
		Default : in Boolean)
		renames IO_Boolean.IO;
	
	-- implementation of mapping
	
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Callback : not null access procedure) is
	begin
		case Object.Direction is
			when Reading =>
				if Name = Next_Name (Object.Reader).all
					and then Next_Kind (Object.Reader) = Enter_Mapping
				then
					Advance (Object.Reader, In_Mapping);
					while Next_Kind (Object.Reader) /= Leave_Mapping loop
						Callback.all;
						Advance_Structure (Object.Reader, In_Mapping);
					end loop;
				end if;
			when Writing =>
				Enter_Mapping (Object.Writer, Name);
				Callback.all;
				Leave_Mapping (Object.Writer);
		end case;
	end IO;
	
	procedure IO (
		Object : not null access Serializer;
		Callback : not null access procedure) is
	begin
		IO (Object, "", Callback);
	end IO;
	
	function Has_Element (Position : Cursor) return Boolean is
	begin
		return Boolean (Position);
	end Has_Element;
	
	function IO (Object : not null access Serializer; Name : String)
		return Mapping_Iterator_Interfaces.Forward_Iterator'Class
	is
		Entry_Presence : Boolean;
	begin
		case Object.Direction is
			when Reading =>
				if Name = Next_Name (Object.Reader).all
					and then Next_Kind (Object.Reader) = Enter_Mapping
				then
					Advance (Object.Reader, In_Mapping);
					Entry_Presence := Next_Kind (Object.Reader) /= Leave_Mapping;
				else
					Entry_Presence := False;
				end if;
			when Writing =>
				Enter_Mapping (Object.Writer, Name);
				Entry_Presence := True;
		end case;
		return Mapping_Iterator'(
			Serializer => Object.all'Unchecked_Access,
			Entry_Presence => Entry_Presence);
	end IO;
	
	function IO (Object : not null access Serializer)
		return Mapping_Iterator_Interfaces.Forward_Iterator'Class is
	begin
		return IO (Object, "");
	end IO;
	
	package body IO_Map_2005 is
		
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Name : in String;
				Item : in out Element_Type))
		is
			procedure Process_Update (Key : in String; Item : in out Element_Type) is
			begin
				Callback (Object, Key, Item);
			end Process_Update;
		begin
			case Object.Direction is
				when Reading =>
					if Name = Next_Name (Object.Reader).all
						and then Next_Kind (Object.Reader) = Enter_Sequence
					then
						Clear (Value);
						Advance (Object.Reader, In_Mapping);
						while Next_Kind (Object.Reader) /= Leave_Sequence loop
							declare
								Position : Cursor;
								Inserted : Boolean;
							begin
								Insert (Value, Next_Name (Object.Reader).all, Default, Position, Inserted);
								if not Inserted then
									raise Constraint_Error;
								end if;
								Update_Element (Value, Position, Process_Update'Access);
							end;
							Advance_Structure (Object.Reader, In_Mapping);
						end loop;
					end if;
				when Writing =>
					Enter_Mapping (Object.Writer, Name);
					declare
						procedure Process_Iterate (Position : in Cursor) is
						begin
							Update_Element (Value, Position, Process_Update'Access);
						end Process_Iterate;
					begin
						Iterate (Value, Process_Iterate'Access);
					end;
					Leave_Mapping (Object.Writer);
			end case;
		end IO;
		
		procedure IO (
			Object : not null access Serializer;
			Value : in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Name : in String;
				Item : in out Element_Type)) is
		begin
			IO (Object, "", Value, Callback);
		end IO;
		
	end IO_Map_2005;
	
	package body IO_Map_2012 is
		
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : aliased in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Name : in String;
				Item : in out Element_Type)) is
		begin
			case Object.Direction is
				when Reading =>
					if Name = Next_Name (Object.Reader).all
						and then Next_Kind (Object.Reader) = Enter_Sequence
					then
						Clear (Value);
						Advance (Object.Reader, In_Mapping);
						while Next_Kind (Object.Reader) /= Leave_Sequence loop
							declare
								Position : Cursor;
								Inserted : Boolean;
							begin
								Insert (Value, Next_Name (Object.Reader).all, Default, Position, Inserted);
								if not Inserted then
									raise Constraint_Error;
								end if;
								Callback (Object, Key (Position), Reference (Value, Position).Element.all);
							end;
							Advance_Structure (Object.Reader, In_Mapping);
						end loop;
					end if;
				when Writing =>
					Enter_Mapping (Object.Writer, Name);
					declare
						I : Cursor := First (Value);
					begin
						while Has_Element (I) loop
							Callback (Object, Key (I), Reference (Value, I).Element.all);
							I := Next (I);
						end loop;
					end;
					Leave_Mapping (Object.Writer);
			end case;
		end IO;
		
		procedure IO (
			Object : not null access Serializer;
			Value : aliased in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Name : in String;
				Item : in out Element_Type)) is
		begin
			IO (Object, "", Value, Callback);
		end IO;
		
	end IO_Map_2012;
	
	-- private implementation of mapping
	
	overriding function First (Object : Mapping_Iterator) return Cursor is
	begin
		return Cursor (Object.Entry_Presence);
	end First;
	
	overriding function Next (Object : Mapping_Iterator; Position : Cursor)
		return Cursor
	is
		pragma Unreferenced (Position);
	begin
		case Object.Serializer.Direction is
			when Reading =>
				Advance_Structure (Object.Serializer.Reader, In_Mapping);
				return Cursor (Next_Kind (Object.Serializer.Reader) /= Leave_Mapping);
			when Writing =>
				Leave_Mapping (Object.Serializer.Writer);
				return False;
		end case;
	end Next;
	
	-- implementation of sequence
	
	package body IO_Array is
		
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Array_Access;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type)) is
		begin
			case Object.Direction is
				when Reading =>
					if Name = Next_Name (Object.Reader).all
						and then Next_Kind (Object.Reader) = Enter_Sequence
					then
						declare
							Last : Index_Type'Base;
						begin
							if Value = null then
								Last := Index_Type'Base'Pred (Index_Type'First);
							else
								Last := Index_Type'Base'Pred (Value'First);
							end if;
							Advance (Object.Reader, In_Sequence);
							while Next_Kind (Object.Reader) /= Leave_Sequence loop
								if Value = null then
									Value :=
										new Array_Type (
											Index_Type'First ..
											Index_Type'Val (
												Natural'Min (
													Index_Type'Pos (Index_Type'Last),
													Index_Type'Pos (Index_Type'First) + 255)));
								elsif Value'Last = Last then
									declare
										subtype Expanding_Range is
											Index_Type'Base range
												Index_Type'Succ (Value'Last) ..
												Index_Type'Val (
													Natural'Min (
														Index_Type'Pos (Index_Type'Last),
														Index_Type'Pos (Value'Last) + 256));
										Copy : constant Array_Access :=
											new Array_Type'(Value.all & Array_Type'(Expanding_Range => <>));
									begin
										Free (Value);
										Value := Copy;
									end;
								end if;
								Last := Index_Type'Succ (Last);
								Value (Last) := Default;
								Callback.all (Object, Value (Last));
								Advance_Structure (Object.Reader, In_Sequence);
							end loop;
							if Value /= null and then Last < Value'Last then
								declare
									Copy : Array_Access := null;
								begin
									if Last >= Value'First then
										Copy := new Array_Type'(Value (Value'First .. Last));
									end if;
									Free (Value);
									Value := Copy;
								end;
							end if;
						end;
					end if;
				when Writing =>
					Enter_Sequence (Object.Writer, Name);
					if Value /= null then
						for I in Value'Range loop
							Callback.all (Object, Value (I));
						end loop;
					end if;
					Leave_Sequence (Object.Writer);
			end case;
		end IO;
		
		procedure IO (
			Object : not null access Serializer;
			Value : in out Array_Access;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type)) is
		begin
			IO (Object, "", Value, Callback);
		end IO;
		
	end IO_Array;
	
	package body IO_List_2005 is
		
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type))
		is
			procedure Process_Update (Item : in out Element_Type) is
			begin
				Callback (Object, Item);
			end Process_Update;
		begin
			case Object.Direction is
				when Reading =>
					if Name = Next_Name (Object.Reader).all
						and then Next_Kind (Object.Reader) = Enter_Sequence
					then
						Clear (Value);
						Advance (Object.Reader, In_Sequence);
						while Next_Kind (Object.Reader) /= Leave_Sequence loop
							Append (Value, Default);
							Update_Element (Value, Last (Value), Process_Update'Access);
							Advance_Structure (Object.Reader, In_Sequence);
						end loop;
					end if;
				when Writing =>
					Enter_Sequence (Object.Writer, Name);
					declare
						procedure Process_Iterate (Position : in Cursor) is
						begin
							Update_Element (Value, Position, Process_Update'Access);
						end Process_Iterate;
					begin
						Iterate (Value, Process_Iterate'Access);
					end;
					Leave_Sequence (Object.Writer);
			end case;
		end IO;
		
		procedure IO (
			Object : not null access Serializer;
			Value : in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type)) is
		begin
			IO (Object, "", Value, Callback);
		end IO;
		
	end IO_List_2005;
	
	package body IO_List_2012 is
		
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : aliased in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type)) is
		begin
			case Object.Direction is
				when Reading =>
					if Name = Next_Name (Object.Reader).all
						and then Next_Kind (Object.Reader) = Enter_Sequence
					then
						Clear (Value);
						Advance (Object.Reader, In_Sequence);
						while Next_Kind (Object.Reader) /= Leave_Sequence loop
							Append (Value, Default);
							declare
								Last_Position : constant Cursor := Last (Value);
							begin
								Callback (Object, Reference (Value, Last_Position).Element.all);
							end;
							Advance_Structure (Object.Reader, In_Sequence);
						end loop;
					end if;
				when Writing =>
					Enter_Sequence (Object.Writer, Name);
					declare
						I : Cursor := First (Value);
					begin
						while Has_Element (I) loop
							Callback (Object, Reference (Value, I).Element.all);
							exit when I = Last (Value); -- for array ???
							I := Next (I);
						end loop;
					end;
					Leave_Sequence (Object.Writer);
			end case;
		end IO;
		
		procedure IO (
			Object : not null access Serializer;
			Value : aliased in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type)) is
		begin
			IO (Object, "", Value, Callback);
		end IO;
		
	end IO_List_2012;
	
	package body IO_Set_2005 is
		
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type)) is
		begin
			case Object.Direction is
				when Reading =>
					if Name = Next_Name (Object.Reader).all
						and then Next_Kind (Object.Reader) = Enter_Sequence
					then
						Clear (Value);
						Advance (Object.Reader, In_Sequence);
						while Next_Kind (Object.Reader) /= Leave_Sequence loop
							declare
								Position : Cursor;
								Inserted : Boolean;
								New_Item : Element_Type := Default;
							begin
								Callback (Object, New_Item);
								Insert (Value, New_Item, Position, Inserted);
								if not Inserted then
									raise Constraint_Error;
								end if;
							end;
							Advance_Structure (Object.Reader, In_Sequence);
						end loop;
					end if;
				when Writing =>
					Enter_Sequence (Object.Writer, Name);
					declare
						procedure Process_Iterate (Position : in Cursor) is
							procedure Process_Query (Item : in Element_Type) is
								Mutable_Item : Element_Type := Item;
							begin
								Callback (Object, Mutable_Item);
							end Process_Query;
						begin
							Query_Element (Position, Process_Query'Access);
						end Process_Iterate;
					begin
						Iterate (Value, Process_Iterate'Access);
					end;
					Leave_Sequence (Object.Writer);
			end case;
		end IO;
		
		procedure IO (
			Object : not null access Serializer;
			Value : in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type)) is
		begin
			IO (Object, "", Value, Callback);
		end IO;
		
	end IO_Set_2005;
	
	package body IO_Set_2012 is
		
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : aliased in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type)) is
		begin
			case Object.Direction is
				when Reading =>
					if Name = Next_Name (Object.Reader).all
						and then Next_Kind (Object.Reader) = Enter_Sequence
					then
						Clear (Value);
						Advance (Object.Reader, In_Sequence);
						while Next_Kind (Object.Reader) /= Leave_Sequence loop
							declare
								Position : Cursor;
								Inserted : Boolean;
								New_Item : Element_Type := Default;
							begin
								Callback (Object, New_Item);
								Insert (Value, New_Item, Position, Inserted);
								if not Inserted then
									raise Constraint_Error;
								end if;
							end;
							Advance_Structure (Object.Reader, In_Sequence);
						end loop;
					end if;
				when Writing =>
					Enter_Sequence (Object.Writer, Name);
					declare
						I : Cursor := First (Value);
					begin
						while Has_Element (I) loop
							declare
								Mutable_Item : Element_Type := Constant_Reference (Value, I).Element.all;
							begin
								Callback (Object, Mutable_Item);
							end;
							I := Next (I);
						end loop;
					end;
					Leave_Sequence (Object.Writer);
			end case;
		end IO;
		
		procedure IO (
			Object : not null access Serializer;
			Value : aliased in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type)) is
		begin
			IO (Object, "", Value, Callback);
		end IO;
		
	end IO_Set_2012;
	
end Serialization;
