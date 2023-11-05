%%%-------------------------------------------------------------------
%%% @author kolpa
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. нояб. 2023 19:24
%%%-------------------------------------------------------------------
-module(protocol).
-author("kolpa").

%% API
-record(ipv4, {
  version,
  ihl,
  tos,
  total_length,
  identification,
  flags,
  frag_offset,
  time_to_live,
  protocol,
  checksum,
  source_address,
  destination_address,
  options_and_padding,
  remaining_data
}).

-export([ipv4/1, ipv4_listener/0]).

ipv4(<<Version:4, IHL:4, ToS:8, TotalLength:16,
  Identification:16, Flags:3, FragOffset:13,
  TimeToLive:8, Protocol:8, Checksum:16,
  SourceAddress:32, DestinationAddress:32,
  OptionsAndPadding:((IHL - 5) * 32)/bits,
  RemainingData/bytes >>) when Version =:= 4 ->
  io:format("Received data ~p ~n", [RemainingData]),
  #ipv4{
    version = Version,
    ihl = IHL,
    tos = ToS,
    total_length = TotalLength,
    identification = Identification,
    flags = Flags,
    frag_offset = FragOffset,
    time_to_live = TimeToLive,
    protocol = Protocol,
    checksum = Checksum,
    source_address = SourceAddress,
    destination_address = DestinationAddress,
    options_and_padding = OptionsAndPadding,
    remaining_data = RemainingData
  };

ipv4(_) -> throw(invalid_data).

ipv4_listener() ->
  receive
    {ipv4, From, BinData} when is_binary(BinData) ->
      Result = protocol:ipv4(BinData),
      From ! Result;
    _ ->
      error(invalid_message_format)
  end.