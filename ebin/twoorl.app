{application, twoorl, [
  {description, "Twoorl is an open-source Twitter clone."},
  {vsn, "0.3"},
  {modules, [twoorl, twoorl_server, twoorl_sup]},
  {registered, [twoorl]},
  {applications, [kernel, stdlib, sasl, crypto, inets, mnesia]},
  {mod, {twoorl, [
      [
        {port, 5001},
        {servername, "twoorl.com"},
        {listen, {0, 0, 0, 0}},
        {docroot, "www"},
        {appmods, [{"/", erlyweb}]},
        {opaque, [{"appname", "twoorl"}]}
      ],
      [
        {port, 5001},
        {servername, "localhost"},
        {listen, {0, 0, 0, 0}},
        {docroot, "www"},
        {appmods, [{"/", erlyweb}]},
        {opaque, [{"appname", "twoorl"}]}
      ]
    ]}},
  {env, []},
  {start_phases, [
    {mysql, [
	{"localhost", "root", "password", "twoorl", 3}
    ]},
    {mnesia, [session]},
    {compile, []}
  ]}
]}.
