%% This is the application resource file (.app file) for the epubnub_chat,
%% application.
{application, epubnub_chat,
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [epubnub_chat_app,
              epubnub_chat_sup,

              epubnub_chat]},
   {registered,[epubnub_chat_sup]},
   {applications, [kernel, stdlib, epubnub]},
   {mod, {epubnub_chat_app,[]}},
   {start_phases, []}]}.

