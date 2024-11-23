module HTMX 
  ( hxGet,
    hxPost,
    hxPut,
    hxDelete,
    hxPatch,
    hxTarget,
    hxSwap,
    hxTrigger,
    hxHeaders,
    hxParams,
    hxInclude,
    hxIndicator,
    hxTimeout,
    hxConfirm,
    hxDisable,
    hxVals,
    hxBoost,
    hxPushUrl,
    hxSelect,
    hxSelectOob,
    hxHistory,
    hxParamsOob,
    hxSync,
    hxReplaceUrl
  ) where

import Text.Blaze.Internal (Attribute, AttributeValue, attribute)
import Text.Blaze.Html5.Attributes 

hxGet :: AttributeValue -> Attribute
hxGet = attribute "hx-get" " hx-get=\""

hxPost :: AttributeValue -> Attribute
hxPost = attribute "hx-post" " hx-post=\""

hxPut :: AttributeValue -> Attribute
hxPut = attribute "hx-put" " hx-put=\""

hxDelete :: AttributeValue -> Attribute
hxDelete = attribute "hx-delete" " hx-delete=\""

hxPatch :: AttributeValue -> Attribute
hxPatch = attribute "hx-patch" " hx-patch=\""

hxTarget :: AttributeValue -> Attribute
hxTarget = attribute "hx-target" " hx-target=\""

hxSwap :: AttributeValue -> Attribute
hxSwap = attribute "hx-swap" " hx-swap=\""

hxTrigger :: AttributeValue -> Attribute
hxTrigger = attribute "hx-trigger" " hx-trigger=\""

hxHeaders :: AttributeValue -> Attribute
hxHeaders = attribute "hx-headers" " hx-headers=\""

hxParams :: AttributeValue -> Attribute
hxParams = attribute "hx-params" " hx-params=\""

hxInclude :: AttributeValue -> Attribute
hxInclude = attribute "hx-include" " hx-include=\""

hxIndicator :: AttributeValue -> Attribute
hxIndicator = attribute "hx-indicator" " hx-indicator=\""

hxTimeout :: AttributeValue -> Attribute
hxTimeout = attribute "hx-timeout" " hx-timeout=\""

hxConfirm :: AttributeValue -> Attribute
hxConfirm = attribute "hx-confirm" " hx-confirm=\""

hxDisable :: AttributeValue -> Attribute
hxDisable = attribute "hx-disable" " hx-disable=\""

hxVals :: AttributeValue -> Attribute
hxVals = attribute "hx-vals" " hx-vals=\""

hxBoost :: AttributeValue -> Attribute
hxBoost = attribute "hx-boost" " hx-boost=\""

hxPushUrl :: AttributeValue -> Attribute
hxPushUrl = attribute "hx-push-url" " hx-push-url=\""

hxSelect :: AttributeValue -> Attribute
hxSelect = attribute "hx-select" " hx-select=\""

hxSelectOob :: AttributeValue -> Attribute
hxSelectOob = attribute "hx-select-oob" " hx-select-oob=\""

hxHistory :: AttributeValue -> Attribute
hxHistory = attribute "hx-history" " hx-history=\""

hxParamsOob :: AttributeValue -> Attribute
hxParamsOob = attribute "hx-params-oob" " hx-params-oob=\""

hxSync :: AttributeValue -> Attribute
hxSync = attribute "hx-sync" " hx-sync=\""

hxReplaceUrl :: AttributeValue -> Attribute
hxReplaceUrl = attribute "hx-replace-url" " hx-replace-url=\""
