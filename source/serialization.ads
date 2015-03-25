pragma Ada_2012;
with Ada.Containers;
with Ada.Strings.Unbounded;
package Serialization is
	pragma Preelaborate;
	
	type Stream_Element_Kind is (
		Value,
		Enter_Mapping,
		Leave_Mapping,
		Enter_Sequence,
		Leave_Sequence,
		End_Of_Stream);
	
	type Direction is (Reading, Writing);
	
	type Serializer (Direction : Serialization.Direction) is limited private;
	
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Value : in out Ada.Strings.Unbounded.Unbounded_String);
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Value : in out Ada.Strings.Unbounded.Unbounded_String;
		Default : in Ada.Strings.Unbounded.Unbounded_String);
	procedure IO (
		Object : not null access Serializer;
		Value : in out Ada.Strings.Unbounded.Unbounded_String);
	procedure IO (
		Object : not null access Serializer;
		Value : in out Ada.Strings.Unbounded.Unbounded_String;
		Default : in Ada.Strings.Unbounded.Unbounded_String);
	
	generic
		type Custom_Type is private;
		with function Image (S : Custom_Type) return String;
		with function Value (S : String) return Custom_Type;
		Triming : in Boolean := False;
	package IO_Custom is
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Custom_Type); 
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Custom_Type;
			Default : in Custom_Type);
	end IO_Custom;
	
	generic
		type Enumeration_Type is (<>);
	package IO_Enumeration is
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Enumeration_Type); 
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Enumeration_Type;
			Default : in Enumeration_Type);
	private
		package Enum_IO is new IO_Custom (
			Enumeration_Type,
			Enumeration_Type'Image,
			Enumeration_Type'Value,
			Triming => True);
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Enumeration_Type)
			renames Enum_IO.IO;
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Enumeration_Type;
			Default : in Enumeration_Type)
			renames Enum_IO.IO;
	end IO_Enumeration;
	
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Value : in out Integer);
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Value : in out Integer;
		Default : in Integer);
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Value : in out Boolean);
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Value : in out Boolean;
		Default : in Boolean);
	
	-- mapping
	
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Callback : not null access procedure);
	procedure IO (
		Object : not null access Serializer;
		Callback : not null access procedure);
	
	generic
		type Cursor is private;
		type Element_Type is private;
		type Container_Type is limited private;
		Default : in Element_Type;
		with procedure Iterate (
			Container : in Container_Type;
			Process : not null access procedure (Position : in Cursor)) is <>;
		with procedure Update_Element (
			Container : in out Container_Type;
			Position : in Cursor;
			Process : not null access procedure (
				Key : in String;
				Element : in out Element_Type)) is <>;
		with procedure Insert (
			Container : in out Container_Type;
			Key : in String;
			New_Item : in Element_Type;
			Position : out Cursor;
			Inserted : out Boolean) is <>;
		with procedure Clear (Container : in out Container_Type) is <>;
	package IO_Map_2005 is
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Name : in String;
				Item : in out Element_Type));
		procedure IO (
			Object : not null access Serializer;
			Value : in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Name : in String;
				Item : in out Element_Type));
	end IO_Map_2005;
	
	generic
		type Cursor is private;
		type Element_Type is private;
		type Container_Type is limited private;
		type Reference_Type (
			Element : not null access Element_Type) is limited private;
		Default : in Element_Type;
		with function Has_Element (Position : Cursor) return Boolean is <>;
		with function First (Container : Container_Type) return Cursor is <>;
		with function Next (Position : Cursor) return Cursor is <>;
		with function Key (Position : Cursor) return String is <>;
		with function Reference (
			Container : aliased in out Container_Type;
			Position : Cursor)
			return Reference_Type is <>;
		with procedure Insert (
			Container : in out Container_Type;
			Key : in String;
			New_Item : in Element_Type;
			Position : out Cursor;
			Inserted : out Boolean) is <>;
		with procedure Clear (Container : in out Container_Type) is <>;
	package IO_Map_2012 is
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Name : in String;
				Item : in out Element_Type));
		procedure IO (
			Object : not null access Serializer;
			Value : in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Name : in String;
				Item : in out Element_Type));
	end IO_Map_2012;
	
	generic package IO_Map
		renames IO_Map_2012;
	
	-- sequence
	
	generic
		type Index_Type is (<>);
		type Element_Type is private;
		type Array_Type is array (Index_Type range <>) of Element_Type;
		type Array_Access is access Array_Type;
		Default : in Element_Type;
		with procedure Free (X : in out Array_Access) is <>;
	package IO_Array is
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Array_Access;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type));
		procedure IO (
			Object : not null access Serializer;
			Value : in out Array_Access;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type));
	end IO_Array;
	
	generic
		type Cursor is private;
		type Element_Type is private;
		type Container_Type is limited private;
		Default : in Element_Type;
		with function Last (Container : Container_Type) return Cursor is <>;
		with procedure Iterate (
			Container : in Container_Type;
			Process : not null access procedure (Position : in Cursor)) is <>;
		with procedure Update_Element (
			Container : in out Container_Type;
			Position : in Cursor;
			Process : not null access procedure (Element : in out Element_Type)) is <>;
		with procedure Append (
			Container : in out Container_Type;
			New_Item : in Element_Type;
			Count : in Ada.Containers.Count_Type := 1) is <>;
		with procedure Clear (Container : in out Container_Type) is <>;
	package IO_List_2005 is
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type));
		procedure IO (
			Object : not null access Serializer;
			Value : in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type));
	end IO_List_2005;
	
	generic
		type Cursor is private;
		type Element_Type is private;
		type Container_Type is limited private;
		type Reference_Type (
			Element : not null access Element_Type) is limited private;
		Default : in Element_Type;
		with function Last (Container : Container_Type) return Cursor is <>;
		with function Has_Element (Position : Cursor) return Boolean is <>;
		with function First (Container : Container_Type) return Cursor is <>;
		with function Next (Position : Cursor) return Cursor is <>;
		with function Reference (
			Container : aliased in out Container_Type;
			Position : Cursor)
			return Reference_Type is <>;
		with procedure Append (
			Container : in out Container_Type;
			New_Item : in Element_Type;
			Count : in Ada.Containers.Count_Type := 1) is <>;
		with procedure Clear (Container : in out Container_Type) is <>;
	package IO_List_2012 is
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type));
		procedure IO (
			Object : not null access Serializer;
			Value : in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type));
	end IO_List_2012;
	
	generic package IO_List
		renames IO_List_2012;
	
	generic
		type Cursor is private;
		type Element_Type is private;
		type Container_Type is limited private;
		Default : in Element_Type;
		with procedure Iterate (
			Container : in Container_Type;
			Process : not null access procedure (Position : in Cursor)) is <>;
		with procedure Query_Element (
			Position : in Cursor;
			Process : not null access procedure (
				Element : in Element_Type)) is <>;
		with procedure Insert (
			Container : in out Container_Type;
			New_Item : in Element_Type;
			Position : out Cursor;
			Inserted : out Boolean) is <>;
		with procedure Clear (Container : in out Container_Type) is <>;
	package IO_Set_2005 is
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type));
		procedure IO (
			Object : not null access Serializer;
			Value : in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type));
	end IO_Set_2005;
	
	generic
		type Cursor is private;
		type Element_Type is private;
		type Container_Type is limited private;
		type Constant_Reference_Type (
			Element : not null access constant Element_Type) is limited private;
		Default : in Element_Type;
		with function Has_Element (Position : Cursor) return Boolean is <>;
		with function First (Container : Container_Type) return Cursor is <>;
		with function Next (Position : Cursor) return Cursor is <>;
		with function Constant_Reference (
			Container : aliased Container_Type;
			Position : Cursor)
			return Constant_Reference_Type is <>;
		with procedure Insert (
			Container : in out Container_Type;
			New_Item : in Element_Type;
			Position : out Cursor;
			Inserted : out Boolean) is <>;
		with procedure Clear (Container : in out Container_Type) is <>;
	package IO_Set_2012 is
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type));
		procedure IO (
			Object : not null access Serializer;
			Value : in out Container_Type;
			Callback : not null access procedure (
				Object : not null access Serializer;
				Item : in out Element_Type));
	end IO_Set_2012;
	
	generic package IO_Set
		renames IO_Set_2012;
	
private
	
	type State is (In_Mapping, In_Sequence);
	pragma Discard_Names (State);
	
	type Reader is abstract tagged limited null record;
	
	function Next_Kind (Object : not null access Reader)
		return Stream_Element_Kind is abstract;
	function Next_Name (Object : not null access Reader)
		return not null access constant String is abstract;
	function Next_Value (Object : not null access Reader)
		return not null access constant String is abstract;
	procedure Advance (
		Object : not null access Reader;
		Position : in State) is abstract;
	procedure Advance_Structure (
		Object : not null access Reader;
		Position : in State);
	
	type Writer is abstract tagged limited null record;
	
	procedure Put (
		Object : not null access Writer;
		Name : in String;
		Item : in String) is abstract;
	procedure Enter_Mapping (
		Object : not null access Writer;
		Name : in String) is abstract;
	procedure Leave_Mapping (Object : not null access Writer) is abstract;
	procedure Enter_Sequence (
		Object : not null access Writer;
		Name : in String) is abstract;
	procedure Leave_Sequence (Object : not null access Writer) is abstract;
	
	type Serializer (Direction : Serialization.Direction) is limited record
		case Direction is
			when Reading =>
				Reader : not null access Serialization.Reader'Class;
			when Writing =>
				Writer : not null access Serialization.Writer'Class;
		end case;
	end record;
	
end Serialization;
