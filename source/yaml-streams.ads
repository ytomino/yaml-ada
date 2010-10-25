with Ada.Streams;
package YAML.Streams is
	pragma Preelaborate;
	
	function Create (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class)
		return Parser;
	
	function Create (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class)
		return Emitter;
	
end YAML.Streams;
