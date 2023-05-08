(window.webpackJsonp=window.webpackJsonp||[]).push([[26],{299:function(t,a,s){"use strict";s.r(a);var e=s(14),n=Object(e.a)({},(function(){var t=this,a=t._self._c;return a("ContentSlotsDistributor",{attrs:{"slot-key":t.$parent.slotKey}},[a("h1",{attrs:{id:"validation"}},[a("a",{staticClass:"header-anchor",attrs:{href:"#validation"}},[t._v("#")]),t._v(" Validation")]),t._v(" "),a("p",[t._v("Caliban provides a little macro called "),a("code",[t._v("gqldoc")]),t._v(" that can check at "),a("strong",[t._v("compile-time")]),t._v(" that a GraphQL query (a "),a("em",[t._v("document")]),t._v(" to be exact) has valid syntax.")]),t._v(" "),a("div",{staticClass:"language-scala mdoc:silent extra-class"},[a("pre",{pre:!0,attrs:{class:"language-scala"}},[a("code",[a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("import")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token namespace"}},[t._v("caliban"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")])]),t._v("Macros"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("gqldoc\n\n"),a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" query "),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" gqldoc"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),a("span",{pre:!0,attrs:{class:"token triple-quoted-string string"}},[t._v('"""\n  query test {\n    amos: character(name: "Amos Burton") {\n      name\n    }\n  }"""')]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n")])])]),a("p",[t._v("At "),a("strong",[t._v("runtime")]),t._v(", it is possible to validate a query against your schema by calling the method "),a("code",[t._v("check")]),t._v(" on your API.")]),t._v(" "),a("div",{staticClass:"language-scala extra-class"},[a("pre",{pre:!0,attrs:{class:"language-scala"}},[a("code",[a("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("def")]),t._v(" check"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("query"),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("String")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),a("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" IO"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),t._v("CalibanError"),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" "),a("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Unit")]),a("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),t._v("\n")])])]),a("p",[t._v("It is also possible to skip validation when executing a query by passing "),a("code",[t._v("Configurator.setSkipValidation(true)")]),t._v(" to the "),a("code",[t._v("configure")]),t._v(" function of your http/ws interpreter. This will slightly improve performance.")])])}),[],!1,null,null,null);a.default=n.exports}}]);