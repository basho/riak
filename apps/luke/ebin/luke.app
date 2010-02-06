% -*- mode: erlang -*-
{application, luke,
 [{description,  "Map/Reduce Framework"},
  {vsn,          "0.1"},
  {modules,      [luke, luke_flow, luke_flow_sup, luke_phase, luke_phase_sup, luke_phases, luke_sup]},
  {registered,   [luke_flow_sup, luke_phase_sup, luke_sup]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {luke, []}}]}.
