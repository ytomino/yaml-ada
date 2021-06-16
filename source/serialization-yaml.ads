with YAML;
private with Ada.Finalization;
package Serialization.YAML is
	pragma Preelaborate;
	
	type Reference_Type (
		Serializer : not null access Serialization.Serializer) is limited private;
	
	function Reading (
		Parser : not null access Standard.YAML.Parser;
		Tag : String)
		return Reference_Type;
	
	function Writing (
		Emitter : not null access Standard.YAML.Emitter;
		Tag : String)
		return Reference_Type;
	
private
	
	type Serializer_Access is access Serializer;
	
	type Reader;
	type Reader_Access is access Reader;
	type Writer;
	type Writer_Access is access Writer;
	
	type Reference_Type (
		Serializer : not null access Serializer) is
		limited new Ada.Finalization.Limited_Controlled
		with record
			Serializer_Body : Serializer_Access;
			Reader_Body : Reader_Access;
			Writer_Body : Writer_Access;
		end record;
	
	overriding procedure Finalize (Object : in out Reference_Type);
	
	-- reading
	
	type Reader is limited new Serialization.Reader
		with record
			Parser : not null access Standard.YAML.Parser;
			Next_Kind : Stream_Element_Kind;
			Next_Name : Ada.Strings.Unbounded.String_Access;
			Next_Value : Ada.Strings.Unbounded.String_Access;
			Level : Natural;
		end record;
	
	overriding function Next_Kind (Object : not null access Reader)
		return Stream_Element_Kind;
	overriding function Next_Name (Object : not null access Reader)
		return not null access constant String;
	overriding function Next_Value (Object : not null access Reader)
		return not null access constant String;
	overriding procedure Advance (
		Object : not null access Reader;
		Position : in State);
	
	-- writing
	
	type Writer is limited new Serialization.Writer
		with record
			Emitter : not null access Standard.YAML.Emitter;
			Tag : Ada.Strings.Unbounded.String_Access;
			Level : Natural;
		end record;
	
	overriding procedure Put (
		Object : not null access Writer;
		Name : in String;
		Item : in String);
	overriding procedure Enter_Mapping (
		Object : not null access Writer;
		Name : in String);
	overriding procedure Leave_Mapping (Object : not null access Writer);
	overriding procedure Enter_Sequence (
		Object : not null access Writer;
		Name : in String);
	overriding procedure Leave_Sequence (Object : not null access Writer);
	
end Serialization.YAML;
