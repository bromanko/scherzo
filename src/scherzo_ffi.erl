-module(scherzo_ffi).
-export([identity/1]).

%% Identity function - returns value unchanged
%% Used for safe type coercion when we know types are compatible
identity(X) -> X.
