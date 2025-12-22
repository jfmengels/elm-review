module TestHelpers exposing (ruleExpectingNaN, ruleWithDefaults)

import Review.Rule exposing (Rule)
import Simplify


ruleWithDefaults : Rule
ruleWithDefaults =
    Simplify.rule Simplify.defaults


ruleExpectingNaN : Rule
ruleExpectingNaN =
    Simplify.rule (Simplify.expectNaN Simplify.defaults)
