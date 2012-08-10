-module (element_anchor).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, anchor).

render_element(Rec = #anchor{}) ->
    wf_tags:emit_tag(a, " ", [{name, wf:to_list(Rec#anchor.name)}]).
