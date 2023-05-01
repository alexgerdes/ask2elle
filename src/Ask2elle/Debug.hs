module Debug where

import Ideas.Common.Library hiding (emptyPrefix, ready, showDerivation)
import Ideas.Common.Library qualified as C
import Ideas.Common.Rewriting.Term
import Ideas.Common.Strategy hiding (emptyPrefix, fail, not)
import Ideas.Common.Strategy.Abstract hiding (emptyPrefix)

{-import Ideas.Common.Utils (allsame, readM)-}

import Ideas.Common.DerivationTree
import Ideas.Common.View
import Ideas.Service.BasicServices qualified as BS
import Ideas.Service.Diagnose hiding (diagnose)
import Ideas.Service.DomainReasoner hiding (exercises)
import Ideas.Service.FeedbackScript.Parser
import Ideas.Service.FeedbackScript.Run
import Ideas.Service.FeedbackScript.Syntax
import Ideas.Service.State
import Ideas.Text.XML
import Ideas.Utils.Uniplate


-- initialise :: String -> IO (State Module, Script, Exercise Module)
-- initialise = initialiseWithPrefix [emptyPath]
