/*******************************************************************************
 * Misc.
 */

// Workaround for missing functionality in IE 8 and earlier.
if( Object.create === undefined ) {
  Object.create = function( o ) {
    function F(){}
    F.prototype = o;
    return new F();
  };
}

// Insert properties of b in place into a.
function Fay$$objConcat(a,b){
  for (var p in b) if (b.hasOwnProperty(p)){
    a[p] = b[p];
  }
  return a;
}

/*******************************************************************************
 * Thunks.
 */

// Force a thunk (if it is a thunk) until WHNF.
function Fay$$_(thunkish,nocache){
  while (thunkish instanceof Fay$$$) {
    thunkish = thunkish.force(nocache);
  }
  return thunkish;
}

// Apply a function to arguments (see method2 in Fay.hs).
function Fay$$__(){
  var f = arguments[0];
  for (var i = 1, len = arguments.length; i < len; i++) {
    f = (f instanceof Fay$$$? Fay$$_(f) : f)(arguments[i]);
  }
  return f;
}

// Thunk object.
function Fay$$$(value){
  this.forced = false;
  this.value = value;
}

// Force the thunk.
Fay$$$.prototype.force = function(nocache) {
  return nocache ?
    this.value() :
    (this.forced ?
     this.value :
     (this.value = this.value(), this.forced = true, this.value));
};


function Fay$$seq(x) {
  return function(y) {
    Fay$$_(x,false);
    return y;
  }
}

function Fay$$seq$36$uncurried(x,y) {
  Fay$$_(x,false);
  return y;
}

/*******************************************************************************
 * Monad.
 */

function Fay$$Monad(value){
  this.value = value;
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then(a){
  return function(b){
    return Fay$$bind(a)(function(_){
      return b;
    });
  };
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then$36$uncurried(a,b){
  return Fay$$bind$36$uncurried(a,function(_){ return b; });
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind(m){
  return function(f){
    return new Fay$$$(function(){
      var monad = Fay$$_(m,true);
      return Fay$$_(f)(monad.value);
    });
  };
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind$36$uncurried(m,f){
  return new Fay$$$(function(){
    var monad = Fay$$_(m,true);
    return Fay$$_(f)(monad.value);
  });
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$$_return(a){
  return new Fay$$Monad(a);
}

// Allow the programmer to access thunk forcing directly.
function Fay$$force(thunk){
  return function(type){
    return new Fay$$$(function(){
      Fay$$_(thunk,type);
      return new Fay$$Monad(Fay$$unit);
    })
  }
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$return$36$uncurried(a){
  return new Fay$$Monad(a);
}

// Unit: ().
var Fay$$unit = null;

/*******************************************************************************
 * Serialization.
 * Fay <-> JS. Should be bijective.
 */

// Serialize a Fay object to JS.
function Fay$$fayToJs(type,fayObj){
  var base = type[0];
  var args = type[1];
  var jsObj;
  if(base == "action") {
    // A nullary monadic action. Should become a nullary JS function.
    // Fay () -> function(){ return ... }
    return function(){
      return Fay$$fayToJs(args[0],Fay$$_(fayObj,true).value);
    };

  }
  else if(base == "function") {
    // A proper function.
    return function(){
      var fayFunc = fayObj;
      var return_type = args[args.length-1];
      var len = args.length;
      // If some arguments.
      if (len > 1) {
        // Apply to all the arguments.
        fayFunc = Fay$$_(fayFunc,true);
        // TODO: Perhaps we should throw an error when JS
        // passes more arguments than Haskell accepts.

        // Unserialize the JS values to Fay for the Fay callback.
        if (args == "automatic_function")
        {
          for (var i = 0; i < arguments.length; i++) {
            fayFunc = Fay$$fayToJs(["automatic"], Fay$$_(fayFunc(Fay$$jsToFay(["automatic"],arguments[i])),true));
          }
          return fayFunc;
        }

        for (var i = 0, len = len; i < len - 1 && fayFunc instanceof Function; i++) {
          fayFunc = Fay$$_(fayFunc(Fay$$jsToFay(args[i],arguments[i])),true);
        }
        // Finally, serialize the Fay return value back to JS.
        var return_base = return_type[0];
        var return_args = return_type[1];
        // If it's a monadic return value, get the value instead.
        if(return_base == "action") {
          return Fay$$fayToJs(return_args[0],fayFunc.value);
        }
        // Otherwise just serialize the value direct.
        else {
          return Fay$$fayToJs(return_type,fayFunc);
        }
      } else {
        throw new Error("Nullary function?");
      }
    };

  }
  else if(base == "string") {
    return Fay$$fayToJs_string(fayObj);
  }
  else if(base == "list") {
    // Serialize Fay list to JavaScript array.
    var arr = [];
    fayObj = Fay$$_(fayObj);
    while(fayObj instanceof Fay$$Cons) {
      arr.push(Fay$$fayToJs(args[0],fayObj.car));
      fayObj = Fay$$_(fayObj.cdr);
    }
    return arr;
  }
  else if(base == "tuple") {
    // Serialize Fay tuple to JavaScript array.
    var arr = [];
    fayObj = Fay$$_(fayObj);
    var i = 0;
    while(fayObj instanceof Fay$$Cons) {
      arr.push(Fay$$fayToJs(args[i++],fayObj.car));
      fayObj = Fay$$_(fayObj.cdr);
    }
    return arr;
  }
  else if(base == "defined") {
    fayObj = Fay$$_(fayObj);
    return fayObj instanceof Fay.FFI._Undefined
      ? undefined
      : Fay$$fayToJs(args[0],fayObj.slot1);
  }
  else if(base == "nullable") {
    fayObj = Fay$$_(fayObj);
    return fayObj instanceof Fay.FFI._Null
      ? null
      : Fay$$fayToJs(args[0],fayObj.slot1);
  }
  else if(base == "double" || base == "int" || base == "bool") {
    // Bools are unboxed.
    return Fay$$_(fayObj);
  }
  else if(base == "ptr" || base == "unknown")
    return fayObj;
  else if(base == "automatic" && fayObj instanceof Function) {
    return Fay$$fayToJs(["function", "automatic_function"], fayObj);
  }
  else if(base == "automatic" || base == "user") {
    fayObj = Fay$$_(fayObj);

    if(fayObj instanceof Fay$$Cons || fayObj === null){
      // Serialize Fay list to JavaScript array.
      var arr = [];
      while(fayObj instanceof Fay$$Cons) {
        arr.push(Fay$$fayToJs(["automatic"],fayObj.car));
        fayObj = Fay$$_(fayObj.cdr);
      }
      return arr;
    } else {
      var fayToJsFun = fayObj && fayObj.instance && Fay$$fayToJsHash[fayObj.instance];
      return fayToJsFun ? fayToJsFun(type,type[2],fayObj) : fayObj;
    }
  }

  throw new Error("Unhandled Fay->JS translation type: " + base);
}

// Stores the mappings from fay types to js objects.
// This will be populated by compiled modules.
var Fay$$fayToJsHash = {};

// Specialized serializer for string.
function Fay$$fayToJs_string(fayObj){
  // Serialize Fay string to JavaScript string.
  var str = "";
  fayObj = Fay$$_(fayObj);
  while(fayObj instanceof Fay$$Cons) {
    str += Fay$$_(fayObj.car);
    fayObj = Fay$$_(fayObj.cdr);
  }
  return str;
};
function Fay$$jsToFay_string(x){
  return Fay$$list(x)
};

// Special num/bool serializers.
function Fay$$jsToFay_int(x){return x;}
function Fay$$jsToFay_double(x){return x;}
function Fay$$jsToFay_bool(x){return x;}

function Fay$$fayToJs_int(x){return Fay$$_(x);}
function Fay$$fayToJs_double(x){return Fay$$_(x);}
function Fay$$fayToJs_bool(x){return Fay$$_(x);}

// Unserialize an object from JS to Fay.
function Fay$$jsToFay(type,jsObj){
  var base = type[0];
  var args = type[1];
  var fayObj;
  if(base == "action") {
    // Unserialize a "monadic" JavaScript return value into a monadic value.
    return new Fay$$Monad(Fay$$jsToFay(args[0],jsObj));
  }
  else if(base == "function") {
    // Unserialize a function from JavaScript to a function that Fay can call.
    // So
    //
    //    var f = function(x,y,z){ … }
    //
    // becomes something like:
    //
    //    function(x){
    //      return function(y){
    //        return function(z){
    //          return new Fay$$$(function(){
    //            return Fay$$jsToFay(f(Fay$$fayTojs(x),
    //                                  Fay$$fayTojs(y),
    //                                  Fay$$fayTojs(z))
    //    }}}}};
    var returnType = args[args.length-1];
    var funArgs = args.slice(0,-1);

    if (jsObj.length > 0) {
      var makePartial = function(args){
        return function(arg){
          var i = args.length;
          var fayArg = Fay$$fayToJs(funArgs[i],arg);
          var newArgs = args.concat([fayArg]);
          if(newArgs.length == funArgs.length) {
            return new Fay$$$(function(){
              return Fay$$jsToFay(returnType,jsObj.apply(this,newArgs));
            });
          } else {
            return makePartial(newArgs);
          }
        };
      };
      return makePartial([]);
    }
    else
      return function (arg) {
        return Fay$$jsToFay(["automatic"], jsObj(Fay$$fayToJs(["automatic"], arg)));
      };
  }
  else if(base == "string") {
    // Unserialize a JS string into Fay list (String).
    // This is a special case, when String is explicit in the type signature,
    // with `Automatic' a string would not be decoded.
    return Fay$$list(jsObj);
  }
  else if(base == "list") {
    // Unserialize a JS array into a Fay list ([a]).
    var serializedList = [];
    for (var i = 0, len = jsObj.length; i < len; i++) {
      // Unserialize each JS value into a Fay value, too.
      serializedList.push(Fay$$jsToFay(args[0],jsObj[i]));
    }
    // Pop it all in a Fay list.
    return Fay$$list(serializedList);
  }
  else if(base == "tuple") {
    // Unserialize a JS array into a Fay tuple ((a,b,c,...)).
    var serializedTuple = [];
    for (var i = 0, len = jsObj.length; i < len; i++) {
      // Unserialize each JS value into a Fay value, too.
      serializedTuple.push(Fay$$jsToFay(args[i],jsObj[i]));
    }
    // Pop it all in a Fay list.
    return Fay$$list(serializedTuple);
  }
  else if(base == "defined") {
    return jsObj === undefined
      ? new Fay.FFI._Undefined()
      : new Fay.FFI._Defined(Fay$$jsToFay(args[0],jsObj));
  }
  else if(base == "nullable") {
    return jsObj === null
      ? new Fay.FFI._Null()
      : new Fay.FFI.Nullable(Fay$$jsToFay(args[0],jsObj));
  }
  else if(base == "int") {
    // Int are unboxed, so there's no forcing to do.
    // But we can do validation that the int has no decimal places.
    // E.g. Math.round(x)!=x? throw "NOT AN INTEGER, GET OUT!"
    fayObj = Math.round(jsObj);
    if(fayObj!==jsObj) throw "Argument " + jsObj + " is not an integer!";
    return fayObj;
  }
  else if (base == "double" ||
           base == "bool" ||
           base ==  "ptr" ||
           base ==  "unknown") {
    return jsObj;
  }
  else if(base == "automatic" && jsObj instanceof Function) {
    var type = [["automatic"]];
    for (var i = 0; i < jsObj.length; i++)
      type.push(["automatic"]);
    return Fay$$jsToFay(["function", type], jsObj);
  }
  else if(base == "automatic" || base == "user") {
    if (jsObj && jsObj['instance']) {
      var jsToFayFun = Fay$$jsToFayHash[jsObj["instance"]];
      return jsToFayFun ? jsToFayFun(type,type[2],jsObj) : jsObj;
    }
    else if (jsObj instanceof Array) {
      var list = null;
      for (var i = jsObj.length - 1; i >= 0; i--) {
        list = new Fay$$Cons(Fay$$jsToFay([base], jsObj[i]), list);
      }
      return list;
    }
    else
      return jsObj;
  }

  throw new Error("Unhandled JS->Fay translation type: " + base);
}

// Stores the mappings from js objects to fay types.
// This will be populated by compiled modules.
var Fay$$jsToFayHash = {};

/*******************************************************************************
 * Lists.
 */

// Cons object.
function Fay$$Cons(car,cdr){
  this.car = car;
  this.cdr = cdr;
}

// Make a list.
function Fay$$list(xs){
  var out = null;
  for(var i=xs.length-1; i>=0;i--)
    out = new Fay$$Cons(xs[i],out);
  return out;
}

// Built-in list cons.
function Fay$$cons(x){
  return function(y){
    return new Fay$$Cons(x,y);
  };
}

// List index.
// `list' is already forced by the time it's passed to this function.
// `list' cannot be null and `index' cannot be out of bounds.
function Fay$$index(index,list){
  for(var i = 0; i < index; i++) {
    list = Fay$$_(list.cdr);
  }
  return list.car;
}

// List length.
// `list' is already forced by the time it's passed to this function.
function Fay$$listLen(list,max){
  for(var i = 0; list !== null && i < max + 1; i++) {
    list = Fay$$_(list.cdr);
  }
  return i == max;
}

/*******************************************************************************
 * Numbers.
 */

// Built-in *.
function Fay$$mult(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) * Fay$$_(y);
    });
  };
}

function Fay$$mult$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) * Fay$$_(y);
  });

}

// Built-in +.
function Fay$$add(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) + Fay$$_(y);
    });
  };
}

// Built-in +.
function Fay$$add$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) + Fay$$_(y);
  });

}

// Built-in -.
function Fay$$sub(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) - Fay$$_(y);
    });
  };
}
// Built-in -.
function Fay$$sub$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) - Fay$$_(y);
  });

}

// Built-in /.
function Fay$$divi(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) / Fay$$_(y);
    });
  };
}

// Built-in /.
function Fay$$divi$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) / Fay$$_(y);
  });

}

/*******************************************************************************
 * Booleans.
 */

// Are two values equal?
function Fay$$equal(lit1, lit2) {
  // Simple case
  lit1 = Fay$$_(lit1);
  lit2 = Fay$$_(lit2);
  if (lit1 === lit2) {
    return true;
  }
  // General case
  if (lit1 instanceof Array) {
    if (lit1.length != lit2.length) return false;
    for (var len = lit1.length, i = 0; i < len; i++) {
      if (!Fay$$equal(lit1[i], lit2[i])) return false;
    }
    return true;
  } else if (lit1 instanceof Fay$$Cons && lit2 instanceof Fay$$Cons) {
    do {
      if (!Fay$$equal(lit1.car,lit2.car))
        return false;
      lit1 = Fay$$_(lit1.cdr), lit2 = Fay$$_(lit2.cdr);
      if (lit1 === null || lit2 === null)
        return lit1 === lit2;
    } while (true);
  } else if (typeof lit1 == 'object' && typeof lit2 == 'object' && lit1 && lit2 &&
             lit1.instance === lit2.instance) {
    for(var x in lit1) {
      if(!Fay$$equal(lit1[x],lit2[x]))
        return false;
    }
    return true;
  } else {
    return false;
  }
}

// Built-in ==.
function Fay$$eq(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$equal(x,y);
    });
  };
}

function Fay$$eq$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$equal(x,y);
  });

}

// Built-in /=.
function Fay$$neq(x){
  return function(y){
    return new Fay$$$(function(){
      return !(Fay$$equal(x,y));
    });
  };
}

// Built-in /=.
function Fay$$neq$36$uncurried(x,y){

  return new Fay$$$(function(){
    return !(Fay$$equal(x,y));
  });

}

// Built-in >.
function Fay$$gt(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) > Fay$$_(y);
    });
  };
}

// Built-in >.
function Fay$$gt$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) > Fay$$_(y);
  });

}

// Built-in <.
function Fay$$lt(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) < Fay$$_(y);
    });
  };
}


// Built-in <.
function Fay$$lt$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) < Fay$$_(y);
  });

}


// Built-in >=.
function Fay$$gte(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) >= Fay$$_(y);
    });
  };
}

// Built-in >=.
function Fay$$gte$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) >= Fay$$_(y);
  });

}

// Built-in <=.
function Fay$$lte(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) <= Fay$$_(y);
    });
  };
}

// Built-in <=.
function Fay$$lte$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) <= Fay$$_(y);
  });

}

// Built-in &&.
function Fay$$and(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) && Fay$$_(y);
    });
  };
}

// Built-in &&.
function Fay$$and$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) && Fay$$_(y);
  });
  ;
}

// Built-in ||.
function Fay$$or(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) || Fay$$_(y);
    });
  };
}

// Built-in ||.
function Fay$$or$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) || Fay$$_(y);
  });

}

/*******************************************************************************
 * Mutable references.
 */

// Make a new mutable reference.
function Fay$$Ref(x){
  this.value = x;
}

// Write to the ref.
function Fay$$writeRef(ref,x){
  ref.value = x;
}

// Get the value from the ref.
function Fay$$readRef(ref,x){
  return ref.value;
}

/*******************************************************************************
 * Dates.
 */
function Fay$$date(str){
  return window.Date.parse(str);
}

/*******************************************************************************
 * Application code.
 */

var Data = {};Data.Data = {};var Fay = {};Fay.FFI = {};Fay.FFI._Nullable = function Nullable(slot1){this.slot1 = slot1;};Fay.FFI._Nullable.prototype.instance = "Nullable";Fay.FFI.Nullable = function(slot1){return new Fay$$$(function(){return new Fay.FFI._Nullable(slot1);});};Fay.FFI._Null = function Null(){};Fay.FFI._Null.prototype.instance = "Null";Fay.FFI.Null = new Fay$$$(function(){return new Fay.FFI._Null();});Fay.FFI._Defined = function Defined(slot1){this.slot1 = slot1;};Fay.FFI._Defined.prototype.instance = "Defined";Fay.FFI.Defined = function(slot1){return new Fay$$$(function(){return new Fay.FFI._Defined(slot1);});};Fay.FFI._Undefined = function Undefined(){};Fay.FFI._Undefined.prototype.instance = "Undefined";Fay.FFI.Undefined = new Fay$$$(function(){return new Fay.FFI._Undefined();});Fay$$objConcat(Fay$$fayToJsHash,{"Nullable": function(type,argTypes,_obj){var obj_ = {"instance": "Nullable"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"Null": function(type,argTypes,_obj){var obj_ = {"instance": "Null"};return obj_;},"Defined": function(type,argTypes,_obj){var obj_ = {"instance": "Defined"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"Undefined": function(type,argTypes,_obj){var obj_ = {"instance": "Undefined"};return obj_;}});Fay$$objConcat(Fay$$jsToFayHash,{"Nullable": function(type,argTypes,obj){return new Fay.FFI._Nullable(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"Null": function(type,argTypes,obj){return new Fay.FFI._Null();},"Defined": function(type,argTypes,obj){return new Fay.FFI._Defined(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"Undefined": function(type,argTypes,obj){return new Fay.FFI._Undefined();}});var Prelude = {};Prelude._Just = function Just(slot1){this.slot1 = slot1;};Prelude._Just.prototype.instance = "Just";Prelude.Just = function(slot1){return new Fay$$$(function(){return new Prelude._Just(slot1);});};Prelude._Nothing = function Nothing(){};Prelude._Nothing.prototype.instance = "Nothing";Prelude.Nothing = new Fay$$$(function(){return new Prelude._Nothing();});Prelude._Left = function Left(slot1){this.slot1 = slot1;};Prelude._Left.prototype.instance = "Left";Prelude.Left = function(slot1){return new Fay$$$(function(){return new Prelude._Left(slot1);});};Prelude._Right = function Right(slot1){this.slot1 = slot1;};Prelude._Right.prototype.instance = "Right";Prelude.Right = function(slot1){return new Fay$$$(function(){return new Prelude._Right(slot1);});};Prelude.maybe = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) instanceof Prelude._Nothing) {var m = $p1;return m;}if (Fay$$_($p3) instanceof Prelude._Just) {var x = Fay$$_($p3).slot1;var f = $p2;return Fay$$_(f)(x);}throw ["unhandled case in maybe",[$p1,$p2,$p3]];});};};};Prelude.$62$$62$$61$ = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$_(Fay$$bind($p1)($p2));});};};Prelude.$62$$62$ = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$_(Fay$$then($p1)($p2));});};};Prelude.$_return = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$return(Fay$$fayToJs(["unknown"],$p1))));});};Prelude.fail = new Fay$$$(function(){return Prelude.error;});Prelude.when = function($p1){return function($p2){return new Fay$$$(function(){var m = $p2;var p = $p1;return Fay$$_(p) ? Fay$$_(Fay$$_(Fay$$then)(m))(Fay$$_(Fay$$$_return)(Fay$$unit)) : Fay$$_(Fay$$$_return)(Fay$$unit);});};};Prelude.forM_ = function($p1){return function($p2){return new Fay$$$(function(){var m = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(m)(x)))(Fay$$_(Fay$$_(Prelude.forM_)(xs))(m));}if (Fay$$_($p1) === null) {return Fay$$_(Fay$$$_return)(Fay$$unit);}throw ["unhandled case in forM_",[$p1,$p2]];});};};Prelude.mapM_ = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var m = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(m)(x)))(Fay$$_(Fay$$_(Prelude.mapM_)(m))(xs));}if (Fay$$_($p2) === null) {return Fay$$_(Fay$$$_return)(Fay$$unit);}throw ["unhandled case in mapM_",[$p1,$p2]];});};};Prelude.$61$$60$$60$ = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$bind)(x))(f);});};};Prelude.sequence = function($p1){return new Fay$$$(function(){var ms = $p1;return (function(){var k = function($p1){return function($p2){return new Fay$$$(function(){var m$39$ = $p2;var m = $p1;return Fay$$_(Fay$$_(Fay$$bind)(m))(function($p1){var x = $p1;return Fay$$_(Fay$$_(Fay$$bind)(m$39$))(function($p1){var xs = $p1;return Fay$$_(Fay$$$_return)(Fay$$_(Fay$$_(Fay$$cons)(x))(xs));});});});};};return Fay$$_(Fay$$_(Fay$$_(Prelude.foldr)(k))(Fay$$_(Fay$$$_return)(null)))(ms);})();});};Prelude.sequence_ = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Fay$$$_return)(Fay$$unit);}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var m = $tmp1.car;var ms = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$then)(m))(Fay$$_(Prelude.sequence_)(ms));}throw ["unhandled case in sequence_",[$p1]];});};Prelude._GT = function GT(){};Prelude._GT.prototype.instance = "GT";Prelude.GT = new Fay$$$(function(){return new Prelude._GT();});Prelude._LT = function LT(){};Prelude._LT.prototype.instance = "LT";Prelude.LT = new Fay$$$(function(){return new Prelude._LT();});Prelude._EQ = function EQ(){};Prelude._EQ.prototype.instance = "EQ";Prelude.EQ = new Fay$$$(function(){return new Prelude._EQ();});Prelude.compare = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(y)) ? Prelude.GT : Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(y)) ? Prelude.LT : Prelude.EQ;});};};Prelude.succ = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$add)(x))(1);});};Prelude.pred = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$sub)(x))(1);});};Prelude.enumFrom = function($p1){return new Fay$$$(function(){var i = $p1;return Fay$$_(Fay$$_(Fay$$cons)(i))(Fay$$_(Prelude.enumFrom)(Fay$$_(Fay$$_(Fay$$add)(i))(1)));});};Prelude.enumFromTo = function($p1){return function($p2){return new Fay$$$(function(){var n = $p2;var i = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(i))(n)) ? null : Fay$$_(Fay$$_(Fay$$cons)(i))(Fay$$_(Fay$$_(Prelude.enumFromTo)(Fay$$_(Fay$$_(Fay$$add)(i))(1)))(n));});};};Prelude.enumFromBy = function($p1){return function($p2){return new Fay$$$(function(){var by = $p2;var fr = $p1;return Fay$$_(Fay$$_(Fay$$cons)(fr))(Fay$$_(Fay$$_(Prelude.enumFromBy)(Fay$$_(Fay$$_(Fay$$add)(fr))(by)))(by));});};};Prelude.enumFromThen = function($p1){return function($p2){return new Fay$$$(function(){var th = $p2;var fr = $p1;return Fay$$_(Fay$$_(Prelude.enumFromBy)(fr))(Fay$$_(Fay$$_(Fay$$sub)(th))(fr));});};};Prelude.enumFromByTo = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var to = $p3;var by = $p2;var fr = $p1;return (function(){var neg = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(to)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(neg)(Fay$$_(Fay$$_(Fay$$add)(x))(by)));});};var pos = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(to)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(pos)(Fay$$_(Fay$$_(Fay$$add)(x))(by)));});};return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(by))(0)) ? Fay$$_(neg)(fr) : Fay$$_(pos)(fr);})();});};};};Prelude.enumFromThenTo = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var to = $p3;var th = $p2;var fr = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.enumFromByTo)(fr))(Fay$$_(Fay$$_(Fay$$sub)(th))(fr)))(to);});};};};Prelude.fromIntegral = function($p1){return new Fay$$$(function(){return $p1;});};Prelude.fromInteger = function($p1){return new Fay$$$(function(){return $p1;});};Prelude.not = function($p1){return new Fay$$$(function(){var p = $p1;return Fay$$_(p) ? false : true;});};Prelude.otherwise = true;Prelude.show = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_string(JSON.stringify(Fay$$fayToJs(["automatic"],$p1)));});};Prelude.error = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],(function() { throw Fay$$fayToJs_string($p1) })());});};Prelude.$_undefined = new Fay$$$(function(){return Fay$$_(Prelude.error)(Fay$$list("Prelude.undefined"));});Prelude.either = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) instanceof Prelude._Left) {var a = Fay$$_($p3).slot1;var f = $p1;return Fay$$_(f)(a);}if (Fay$$_($p3) instanceof Prelude._Right) {var b = Fay$$_($p3).slot1;var g = $p2;return Fay$$_(g)(b);}throw ["unhandled case in either",[$p1,$p2,$p3]];});};};};Prelude.until = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var x = $p3;var f = $p2;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? x : Fay$$_(Fay$$_(Fay$$_(Prelude.until)(p))(f))(Fay$$_(f)(x));});};};};Prelude.$36$$33$ = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$seq)(x))(Fay$$_(f)(x));});};};Prelude.$_const = function($p1){return function($p2){return new Fay$$$(function(){var a = $p1;return a;});};};Prelude.id = function($p1){return new Fay$$$(function(){var x = $p1;return x;});};Prelude.$46$ = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var x = $p3;var g = $p2;var f = $p1;return Fay$$_(f)(Fay$$_(g)(x));});};};};Prelude.$36$ = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(f)(x);});};};Prelude.flip = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var y = $p3;var x = $p2;var f = $p1;return Fay$$_(Fay$$_(f)(y))(x);});};};};Prelude.curry = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var y = $p3;var x = $p2;var f = $p1;return Fay$$_(f)(Fay$$list([x,y]));});};};};Prelude.uncurry = function($p1){return function($p2){return new Fay$$$(function(){var p = $p2;var f = $p1;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var x = Fay$$index(0,Fay$$_($tmp1));var y = Fay$$index(1,Fay$$_($tmp1));return Fay$$_(Fay$$_(f)(x))(y);}return (function(){ throw (["unhandled case",$tmp1]); })();})(p);});};};Prelude.snd = function($p1){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p1),2)) {var x = Fay$$index(1,Fay$$_($p1));return x;}throw ["unhandled case in snd",[$p1]];});};Prelude.fst = function($p1){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p1),2)) {var x = Fay$$index(0,Fay$$_($p1));return x;}throw ["unhandled case in fst",[$p1]];});};Prelude.div = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude.quot)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y)))(1);} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude.quot)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y)))(1);}}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude.quot)(x))(y);});};};Prelude.mod = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude.rem)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y)))(y)))(1);} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude.rem)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y)))(y)))(1);}}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude.rem)(x))(y);});};};Prelude.divMod = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var q = Fay$$index(0,Fay$$_($tmp1));var r = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$sub)(q))(1),Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$add)(r))(y)))(1)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.quotRem)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y));} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(1)))) {return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var q = Fay$$index(0,Fay$$_($tmp1));var r = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$sub)(q))(1),Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Fay$$add)(r))(y)))(1)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.quotRem)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y));}}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude.quotRem)(x))(y);});};};Prelude.min = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],Math.min(Fay$$_(Fay$$fayToJs(["unknown"],$p1)),Fay$$_(Fay$$fayToJs(["unknown"],$p2))));});};};Prelude.max = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],Math.max(Fay$$_(Fay$$fayToJs(["unknown"],$p1)),Fay$$_(Fay$$fayToJs(["unknown"],$p2))));});};};Prelude.recip = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(1))(x);});};Prelude.negate = function($p1){return new Fay$$$(function(){var x = $p1;return (-(Fay$$_(x)));});};Prelude.abs = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(0)) ? Fay$$_(Prelude.negate)(x) : x;});};Prelude.signum = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(0)) ? 1 : Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(x))(0)) ? 0 : (-(1));});};Prelude.pi = new Fay$$$(function(){return Fay$$jsToFay_double(Math.PI);});Prelude.exp = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.exp(Fay$$fayToJs_double($p1)));});};Prelude.sqrt = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.sqrt(Fay$$fayToJs_double($p1)));});};Prelude.log = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.log(Fay$$fayToJs_double($p1)));});};Prelude.$42$$42$ = new Fay$$$(function(){return Prelude.unsafePow;});Prelude.$94$$94$ = new Fay$$$(function(){return Prelude.unsafePow;});Prelude.unsafePow = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],Math.pow(Fay$$_(Fay$$fayToJs(["unknown"],$p1)),Fay$$_(Fay$$fayToJs(["unknown"],$p2))));});};};Prelude.$94$ = function($p1){return function($p2){return new Fay$$$(function(){var b = $p2;var a = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(b))(0))) {return Fay$$_(Prelude.error)(Fay$$list("(^): negative exponent"));} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(b))(0))) {return 1;} else {if (Fay$$_(Fay$$_(Prelude.even)(b))) {return (function(){return new Fay$$$(function(){var x = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude.$94$)(a))(Fay$$_(Fay$$_(Prelude.quot)(b))(2));});return Fay$$_(Fay$$_(Fay$$mult)(x))(x);});})();}}}var b = $p2;var a = $p1;return Fay$$_(Fay$$_(Fay$$mult)(a))(Fay$$_(Fay$$_(Prelude.$94$)(a))(Fay$$_(Fay$$_(Fay$$sub)(b))(1)));});};};Prelude.logBase = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var b = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Prelude.log)(x)))(Fay$$_(Prelude.log)(b));});};};Prelude.sin = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.sin(Fay$$fayToJs_double($p1)));});};Prelude.tan = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.tan(Fay$$fayToJs_double($p1)));});};Prelude.cos = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.cos(Fay$$fayToJs_double($p1)));});};Prelude.asin = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.asin(Fay$$fayToJs_double($p1)));});};Prelude.atan = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.atan(Fay$$fayToJs_double($p1)));});};Prelude.acos = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.acos(Fay$$fayToJs_double($p1)));});};Prelude.sinh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Prelude.exp)(x)))(Fay$$_(Prelude.exp)((-(Fay$$_(x)))))))(2);});};Prelude.tanh = function($p1){return new Fay$$$(function(){var x = $p1;return (function(){return new Fay$$$(function(){var a = new Fay$$$(function(){return Fay$$_(Prelude.exp)(x);});var b = new Fay$$$(function(){return Fay$$_(Prelude.exp)((-(Fay$$_(x))));});return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$sub)(a))(b)))(Fay$$_(Fay$$_(Fay$$add)(a))(b));});})();});};Prelude.cosh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Prelude.exp)(x)))(Fay$$_(Prelude.exp)((-(Fay$$_(x)))))))(2);});};Prelude.asinh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Prelude.log)(Fay$$_(Fay$$_(Fay$$add)(x))(Fay$$_(Prelude.sqrt)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude.$42$$42$)(x))(2)))(1))));});};Prelude.atanh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Prelude.log)(Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$add)(1))(x)))(Fay$$_(Fay$$_(Fay$$sub)(1))(x)))))(2);});};Prelude.acosh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Prelude.log)(Fay$$_(Fay$$_(Fay$$add)(x))(Fay$$_(Prelude.sqrt)(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude.$42$$42$)(x))(2)))(1))));});};Prelude.properFraction = function($p1){return new Fay$$$(function(){var x = $p1;return (function(){return new Fay$$$(function(){var a = new Fay$$$(function(){return Fay$$_(Prelude.truncate)(x);});return Fay$$list([a,Fay$$_(Fay$$_(Fay$$sub)(x))(Fay$$_(Prelude.fromIntegral)(a))]);});})();});};Prelude.truncate = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(0)) ? Fay$$_(Prelude.ceiling)(x) : Fay$$_(Prelude.floor)(x);});};Prelude.round = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_int(Math.round(Fay$$fayToJs_double($p1)));});};Prelude.ceiling = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_int(Math.ceil(Fay$$fayToJs_double($p1)));});};Prelude.floor = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_int(Math.floor(Fay$$fayToJs_double($p1)));});};Prelude.subtract = new Fay$$$(function(){return Fay$$_(Prelude.flip)(Fay$$sub);});Prelude.even = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$eq)(Fay$$_(Fay$$_(Prelude.rem)(x))(2)))(0);});};Prelude.odd = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Prelude.not)(Fay$$_(Prelude.even)(x));});};Prelude.gcd = function($p1){return function($p2){return new Fay$$$(function(){var b = $p2;var a = $p1;return (function(){var go = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === 0) {var x = $p1;return x;}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(go)(y))(Fay$$_(Fay$$_(Prelude.rem)(x))(y));});};};return Fay$$_(Fay$$_(go)(Fay$$_(Prelude.abs)(a)))(Fay$$_(Prelude.abs)(b));})();});};};Prelude.quot = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(y))(0)) ? Fay$$_(Prelude.error)(Fay$$list("Division by zero")) : Fay$$_(Fay$$_(Prelude.quot$39$)(x))(y);});};};Prelude.quot$39$ = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay_int(~~(Fay$$fayToJs_int($p1)/Fay$$fayToJs_int($p2)));});};};Prelude.quotRem = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$list([Fay$$_(Fay$$_(Prelude.quot)(x))(y),Fay$$_(Fay$$_(Prelude.rem)(x))(y)]);});};};Prelude.rem = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(y))(0)) ? Fay$$_(Prelude.error)(Fay$$list("Division by zero")) : Fay$$_(Fay$$_(Prelude.rem$39$)(x))(y);});};};Prelude.rem$39$ = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay_int(Fay$$fayToJs_int($p1) % Fay$$fayToJs_int($p2));});};};Prelude.lcm = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === 0) {return 0;}if (Fay$$_($p1) === 0) {return 0;}var b = $p2;var a = $p1;return Fay$$_(Prelude.abs)(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Prelude.quot)(a))(Fay$$_(Fay$$_(Prelude.gcd)(a))(b))))(b));});};};Prelude.find = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Prelude.Just)(x) : Fay$$_(Fay$$_(Prelude.find)(p))(xs);}if (Fay$$_($p2) === null) {return Prelude.Nothing;}throw ["unhandled case in find",[$p1,$p2]];});};};Prelude.filter = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.filter)(p))(xs)) : Fay$$_(Fay$$_(Prelude.filter)(p))(xs);}if (Fay$$_($p2) === null) {return null;}throw ["unhandled case in filter",[$p1,$p2]];});};};Prelude.$_null = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return true;}return false;});};Prelude.map = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(f)(x)))(Fay$$_(Fay$$_(Prelude.map)(f))(xs));}throw ["unhandled case in map",[$p1,$p2]];});};};Prelude.nub = function($p1){return new Fay$$$(function(){var ls = $p1;return Fay$$_(Fay$$_(Prelude.nub$39$)(ls))(null);});};Prelude.nub$39$ = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return null;}var ls = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$_(Prelude.elem)(x))(ls)) ? Fay$$_(Fay$$_(Prelude.nub$39$)(xs))(ls) : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.nub$39$)(xs))(Fay$$_(Fay$$_(Fay$$cons)(x))(ls)));}throw ["unhandled case in nub'",[$p1,$p2]];});};};Prelude.elem = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var y = $tmp1.car;var ys = $tmp1.cdr;var x = $p1;return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(Fay$$_(Fay$$eq)(x))(y)))(Fay$$_(Fay$$_(Prelude.elem)(x))(ys));}if (Fay$$_($p2) === null) {return false;}throw ["unhandled case in elem",[$p1,$p2]];});};};Prelude.notElem = function($p1){return function($p2){return new Fay$$$(function(){var ys = $p2;var x = $p1;return Fay$$_(Prelude.not)(Fay$$_(Fay$$_(Prelude.elem)(x))(ys));});};};Prelude.sort = new Fay$$$(function(){return Fay$$_(Prelude.sortBy)(Prelude.compare);});Prelude.sortBy = function($p1){return new Fay$$$(function(){var cmp = $p1;return Fay$$_(Fay$$_(Prelude.foldr)(Fay$$_(Prelude.insertBy)(cmp)))(null);});};Prelude.insertBy = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var x = $p2;return Fay$$list([x]);}var ys = $p3;var x = $p2;var cmp = $p1;return (function($tmp1){if (Fay$$_($tmp1) === null) {return Fay$$list([x]);}var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var y = $tmp2.car;var ys$39$ = $tmp2.cdr;return (function($tmp2){if (Fay$$_($tmp2) instanceof Prelude._GT) {return Fay$$_(Fay$$_(Fay$$cons)(y))(Fay$$_(Fay$$_(Fay$$_(Prelude.insertBy)(cmp))(x))(ys$39$));}return Fay$$_(Fay$$_(Fay$$cons)(x))(ys);})(Fay$$_(Fay$$_(cmp)(x))(y));}return (function(){ throw (["unhandled case",$tmp1]); })();})(ys);});};};};Prelude.conc = function($p1){return function($p2){return new Fay$$$(function(){var ys = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.conc)(xs))(ys));}var ys = $p2;if (Fay$$_($p1) === null) {return ys;}throw ["unhandled case in conc",[$p1,$p2]];});};};Prelude.concat = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude.foldr)(Prelude.conc))(null);});Prelude.concatMap = function($p1){return new Fay$$$(function(){var f = $p1;return Fay$$_(Fay$$_(Prelude.foldr)(Fay$$_(Fay$$_(Prelude.$46$)(Prelude.$43$$43$))(f)))(null);});};Prelude.foldr = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var z = $p2;return z;}var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return Fay$$_(Fay$$_(f)(x))(Fay$$_(Fay$$_(Fay$$_(Prelude.foldr)(f))(z))(xs));}throw ["unhandled case in foldr",[$p1,$p2,$p3]];});};};};Prelude.foldr1 = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p2),1)) {var x = Fay$$index(0,Fay$$_($p2));return x;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(f)(x))(Fay$$_(Fay$$_(Prelude.foldr1)(f))(xs));}if (Fay$$_($p2) === null) {return Fay$$_(Prelude.error)(Fay$$list("foldr1: empty list"));}throw ["unhandled case in foldr1",[$p1,$p2]];});};};Prelude.foldl = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var z = $p2;return z;}var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.foldl)(f))(Fay$$_(Fay$$_(f)(z))(x)))(xs);}throw ["unhandled case in foldl",[$p1,$p2,$p3]];});};};};Prelude.foldl1 = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.foldl)(f))(x))(xs);}if (Fay$$_($p2) === null) {return Fay$$_(Prelude.error)(Fay$$list("foldl1: empty list"));}throw ["unhandled case in foldl1",[$p1,$p2]];});};};Prelude.$43$$43$ = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude.conc)(x))(y);});};};Prelude.$33$$33$ = function($p1){return function($p2){return new Fay$$$(function(){var b = $p2;var a = $p1;return (function(){var go = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("(!!): index too large"));}if (Fay$$_($p2) === 0) {var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;return h;}}var n = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return Fay$$_(Fay$$_(go)(t))(Fay$$_(Fay$$_(Fay$$sub)(n))(1));}throw ["unhandled case in go",[$p1,$p2]];});};};return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(b))(0)) ? Fay$$_(Prelude.error)(Fay$$list("(!!): negative index")) : Fay$$_(Fay$$_(go)(a))(b);})();});};};Prelude.head = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("head: empty list"));}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;return h;}throw ["unhandled case in head",[$p1]];});};Prelude.tail = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("tail: empty list"));}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return t;}throw ["unhandled case in tail",[$p1]];});};Prelude.init = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("init: empty list"));}if (Fay$$listLen(Fay$$_($p1),1)) {var a = Fay$$index(0,Fay$$_($p1));return null;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;var t = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(h))(Fay$$_(Prelude.init)(t));}throw ["unhandled case in init",[$p1]];});};Prelude.last = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("last: empty list"));}if (Fay$$listLen(Fay$$_($p1),1)) {var a = Fay$$index(0,Fay$$_($p1));return a;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return Fay$$_(Prelude.last)(t);}throw ["unhandled case in last",[$p1]];});};Prelude.iterate = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.iterate)(f))(Fay$$_(f)(x)));});};};Prelude.repeat = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Prelude.repeat)(x));});};Prelude.replicate = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === 0) {return null;}var x = $p2;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.replicate)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(x));});};};Prelude.cycle = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("cycle: empty list"));}var xs = $p1;return (function(){var xs$39$ = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude.$43$$43$)(xs))(xs$39$);});return xs$39$;})();});};Prelude.take = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === 0) {return null;}if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.take)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs));}throw ["unhandled case in take",[$p1,$p2]];});};};Prelude.drop = function($p1){return function($p2){return new Fay$$$(function(){var xs = $p2;if (Fay$$_($p1) === 0) {return xs;}if (Fay$$_($p2) === null) {return null;}var xss = $p2;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? xss : Fay$$_(Fay$$_(Prelude.drop)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs);}throw ["unhandled case in drop",[$p1,$p2]];});};};Prelude.splitAt = function($p1){return function($p2){return new Fay$$$(function(){var xs = $p2;if (Fay$$_($p1) === 0) {return Fay$$list([null,xs]);}if (Fay$$_($p2) === null) {return Fay$$list([null,null]);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? Fay$$list([null,Fay$$_(Fay$$_(Fay$$cons)(x))(xs)]) : (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));var b = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(a),b]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.splitAt)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs));}throw ["unhandled case in splitAt",[$p1,$p2]];});};};Prelude.takeWhile = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.takeWhile)(p))(xs)) : null;}throw ["unhandled case in takeWhile",[$p1,$p2]];});};};Prelude.dropWhile = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Prelude.dropWhile)(p))(xs) : Fay$$_(Fay$$_(Fay$$cons)(x))(xs);}throw ["unhandled case in dropWhile",[$p1,$p2]];});};};Prelude.span = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return Fay$$list([null,null]);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));var b = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(a),b]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.span)(p))(xs)) : Fay$$list([null,Fay$$_(Fay$$_(Fay$$cons)(x))(xs)]);}throw ["unhandled case in span",[$p1,$p2]];});};};Prelude.$_break = function($p1){return new Fay$$$(function(){var p = $p1;return Fay$$_(Prelude.span)(Fay$$_(Fay$$_(Prelude.$46$)(Prelude.not))(p));});};Prelude.zipWith = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(a))(b)))(Fay$$_(Fay$$_(Fay$$_(Prelude.zipWith)(f))(as))(bs));}}return null;});};};};Prelude.zipWith3 = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){var $tmp1 = Fay$$_($p4);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Fay$$_(f)(a))(b))(c)))(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Prelude.zipWith3)(f))(as))(bs))(cs));}}}return null;});};};};};Prelude.zip = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$list([a,b])))(Fay$$_(Fay$$_(Prelude.zip)(as))(bs));}}return null;});};};Prelude.zip3 = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$list([a,b,c])))(Fay$$_(Fay$$_(Fay$$_(Prelude.zip3)(as))(bs))(cs));}}}return null;});};};};Prelude.unzip = function($p1){return new Fay$$$(function(){var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(Fay$$_($tmp1.car),2)) {var x = Fay$$index(0,Fay$$_($tmp1.car));var y = Fay$$index(1,Fay$$_($tmp1.car));var ps = $tmp1.cdr;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var xs = Fay$$index(0,Fay$$_($tmp1));var ys = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Prelude.unzip)(ps));}}if (Fay$$_($p1) === null) {return Fay$$list([null,null]);}throw ["unhandled case in unzip",[$p1]];});};Prelude.unzip3 = function($p1){return new Fay$$$(function(){var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(Fay$$_($tmp1.car),3)) {var x = Fay$$index(0,Fay$$_($tmp1.car));var y = Fay$$index(1,Fay$$_($tmp1.car));var z = Fay$$index(2,Fay$$_($tmp1.car));var ps = $tmp1.cdr;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),3)) {var xs = Fay$$index(0,Fay$$_($tmp1));var ys = Fay$$index(1,Fay$$_($tmp1));var zs = Fay$$index(2,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys),Fay$$_(Fay$$_(Fay$$cons)(z))(zs)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Prelude.unzip3)(ps));}}if (Fay$$_($p1) === null) {return Fay$$list([null,null,null]);}throw ["unhandled case in unzip3",[$p1]];});};Prelude.lines = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return null;}var s = $p1;return (function(){var isLineBreak = function($p1){return new Fay$$$(function(){var c = $p1;return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(Fay$$_(Fay$$eq)(c))("\r")))(Fay$$_(Fay$$_(Fay$$eq)(c))("\n"));});};return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));if (Fay$$_(Fay$$index(1,Fay$$_($tmp1))) === null) {return Fay$$list([a]);}var a = Fay$$index(0,Fay$$_($tmp1));var $tmp2 = Fay$$_(Fay$$index(1,Fay$$_($tmp1)));if ($tmp2 instanceof Fay$$Cons) {var cs = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$cons)(a))(Fay$$_(Prelude.lines)(cs));}}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.$_break)(isLineBreak))(s));})();});};Prelude.unlines = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return null;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var l = $tmp1.car;var ls = $tmp1.cdr;return Fay$$_(Fay$$_(Prelude.$43$$43$)(l))(Fay$$_(Fay$$_(Fay$$cons)("\n"))(Fay$$_(Prelude.unlines)(ls)));}throw ["unhandled case in unlines",[$p1]];});};Prelude.words = function($p1){return new Fay$$$(function(){var str = $p1;return (function(){var words$39$ = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return null;}var s = $p1;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));var b = Fay$$index(1,Fay$$_($tmp1));return Fay$$_(Fay$$_(Fay$$cons)(a))(Fay$$_(Prelude.words)(b));}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.$_break)(isSpace))(s));});};var isSpace = function($p1){return new Fay$$$(function(){var c = $p1;return Fay$$_(Fay$$_(Prelude.elem)(c))(Fay$$list(" \t\r\n\u000c\u000b"));});};return Fay$$_(words$39$)(Fay$$_(Fay$$_(Prelude.dropWhile)(isSpace))(str));})();});};Prelude.unwords = new Fay$$$(function(){return Fay$$_(Prelude.intercalate)(Fay$$list(" "));});Prelude.and = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return true;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$and)(x))(Fay$$_(Prelude.and)(xs));}throw ["unhandled case in and",[$p1]];});};Prelude.or = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return false;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$or)(x))(Fay$$_(Prelude.or)(xs));}throw ["unhandled case in or",[$p1]];});};Prelude.any = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return false;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(p)(x)))(Fay$$_(Fay$$_(Prelude.any)(p))(xs));}throw ["unhandled case in any",[$p1,$p2]];});};};Prelude.all = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return true;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(Fay$$and)(Fay$$_(p)(x)))(Fay$$_(Fay$$_(Prelude.all)(p))(xs));}throw ["unhandled case in all",[$p1,$p2]];});};};Prelude.intersperse = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var sep = $p1;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.prependToAll)(sep))(xs));}throw ["unhandled case in intersperse",[$p1,$p2]];});};};Prelude.prependToAll = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var sep = $p1;return Fay$$_(Fay$$_(Fay$$cons)(sep))(Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.prependToAll)(sep))(xs)));}throw ["unhandled case in prependToAll",[$p1,$p2]];});};};Prelude.intercalate = function($p1){return function($p2){return new Fay$$$(function(){var xss = $p2;var xs = $p1;return Fay$$_(Prelude.concat)(Fay$$_(Fay$$_(Prelude.intersperse)(xs))(xss));});};};Prelude.maximum = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("maximum: empty list"));}var xs = $p1;return Fay$$_(Fay$$_(Prelude.foldl1)(Prelude.max))(xs);});};Prelude.minimum = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("minimum: empty list"));}var xs = $p1;return Fay$$_(Fay$$_(Prelude.foldl1)(Prelude.min))(xs);});};Prelude.product = function($p1){return new Fay$$$(function(){var xs = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.foldl)(Fay$$mult))(1))(xs);});};Prelude.sum = function($p1){return new Fay$$$(function(){var xs = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.foldl)(Fay$$add))(0))(xs);});};Prelude.scanl = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var l = $p3;var z = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(z))((function($tmp1){if (Fay$$_($tmp1) === null) {return null;}var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var x = $tmp2.car;var xs = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$_(Prelude.scanl)(f))(Fay$$_(Fay$$_(f)(z))(x)))(xs);}return (function(){ throw (["unhandled case",$tmp1]); })();})(l));});};};};Prelude.scanl1 = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.scanl)(f))(x))(xs);}throw ["unhandled case in scanl1",[$p1,$p2]];});};};Prelude.scanr = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var z = $p2;return Fay$$list([z]);}var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return (function($tmp1){var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var h = $tmp2.car;var t = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(x))(h)))(Fay$$_(Fay$$_(Fay$$cons)(h))(t));}return Prelude.$_undefined;})(Fay$$_(Fay$$_(Fay$$_(Prelude.scanr)(f))(z))(xs));}throw ["unhandled case in scanr",[$p1,$p2,$p3]];});};};};Prelude.scanr1 = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}if (Fay$$listLen(Fay$$_($p2),1)) {var x = Fay$$index(0,Fay$$_($p2));return Fay$$list([x]);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return (function($tmp1){var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var h = $tmp2.car;var t = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(x))(h)))(Fay$$_(Fay$$_(Fay$$cons)(h))(t));}return Prelude.$_undefined;})(Fay$$_(Fay$$_(Prelude.scanr1)(f))(xs));}throw ["unhandled case in scanr1",[$p1,$p2]];});};};Prelude.lookup = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {var _key = $p1;return Prelude.Nothing;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(Fay$$_($tmp1.car),2)) {var x = Fay$$index(0,Fay$$_($tmp1.car));var y = Fay$$index(1,Fay$$_($tmp1.car));var xys = $tmp1.cdr;var key = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(key))(x)) ? Fay$$_(Prelude.Just)(y) : Fay$$_(Fay$$_(Prelude.lookup)(key))(xys);}}throw ["unhandled case in lookup",[$p1,$p2]];});};};Prelude.length = function($p1){return new Fay$$$(function(){var xs = $p1;return Fay$$_(Fay$$_(Prelude.length$39$)(0))(xs);});};Prelude.length$39$ = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var xs = $tmp1.cdr;var acc = $p1;return Fay$$_(Fay$$_(Prelude.length$39$)(Fay$$_(Fay$$_(Fay$$add)(acc))(1)))(xs);}var acc = $p1;return acc;});};};Prelude.reverse = function($p1){return new Fay$$$(function(){var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Prelude.$43$$43$)(Fay$$_(Prelude.reverse)(xs)))(Fay$$list([x]));}if (Fay$$_($p1) === null) {return null;}throw ["unhandled case in reverse",[$p1]];});};Prelude.print = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs(["automatic"],$p1))));});};Prelude.putStrLn = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs_string($p1))));});};Prelude.ifThenElse = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var b = $p3;var a = $p2;var p = $p1;return Fay$$_(p) ? a : b;});};};};Fay$$objConcat(Fay$$fayToJsHash,{"Just": function(type,argTypes,_obj){var obj_ = {"instance": "Just"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"Nothing": function(type,argTypes,_obj){var obj_ = {"instance": "Nothing"};return obj_;},"Left": function(type,argTypes,_obj){var obj_ = {"instance": "Left"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"Right": function(type,argTypes,_obj){var obj_ = {"instance": "Right"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[1] ? (argTypes)[1] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"GT": function(type,argTypes,_obj){var obj_ = {"instance": "GT"};return obj_;},"LT": function(type,argTypes,_obj){var obj_ = {"instance": "LT"};return obj_;},"EQ": function(type,argTypes,_obj){var obj_ = {"instance": "EQ"};return obj_;}});Fay$$objConcat(Fay$$jsToFayHash,{"Just": function(type,argTypes,obj){return new Prelude._Just(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"Nothing": function(type,argTypes,obj){return new Prelude._Nothing();},"Left": function(type,argTypes,obj){return new Prelude._Left(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"Right": function(type,argTypes,obj){return new Prelude._Right(Fay$$jsToFay(argTypes && (argTypes)[1] ? (argTypes)[1] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"GT": function(type,argTypes,obj){return new Prelude._GT();},"LT": function(type,argTypes,obj){return new Prelude._LT();},"EQ": function(type,argTypes,obj){return new Prelude._EQ();}});var FFI = {};var JS = {};JS.appendChild = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],((Fay$$fayToJs(["user","JSDOM",[]],$p1)).appendChild(Fay$$fayToJs(["user","JSDOM",[]],$p2)))));});};};JS.gid = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["nullable",[["user","JSDOM",[]]]],(document.getElementById(Fay$$fayToJs_string($p1)))));});};JS.mknode = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","JSDOM",[]],(document.createElement(Fay$$fayToJs_string($p1)))));});};JS.setText = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],((Fay$$fayToJs(["user","JSDOM",[]],$p1)).innerText = Fay$$fayToJs_string($p2))));});};};JS.getText = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay_string(((Fay$$fayToJs(["user","JSDOM",[]],$p1)).innerText)));});};JS.alert = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],(alert(Fay$$fayToJs_string($p1)))));});};JS.alertI = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],(alert(Fay$$fayToJs_int($p1)))));});};JS.alertN = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],(alert(Fay$$fayToJs(["user","JSDOM",[]],$p1)))));});};JS.setAttr = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],((Fay$$fayToJs(["user","JSDOM",[]],$p1)).setAttribute(Fay$$fayToJs_string($p2),Fay$$fayToJs_string($p3)))));});};};};JS.$_toString = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay_string(((Fay$$fayToJs(["list",[["user","JSDOM",[]]]],$p1)).toString)));});};JS.getChilds = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["list",[["user","JSDOM",[]]]],((Fay$$fayToJs(["user","JSDOM",[]],$p1)).children)));});};JS.nodeName = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay_string(((Fay$$fayToJs(["user","JSDOM",[]],$p1)).nodeName)));});};JS.removeChild = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],((Fay$$fayToJs(["user","JSDOM",[]],$p1)).removeChild(Fay$$fayToJs(["user","JSDOM",[]],$p2)))));});};};JS.prompt = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["nullable",[["string"]]],(prompt(Fay$$fayToJs_string($p1),Fay$$fayToJs_string($p2)))));});};};JS.getId = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["nullable",[["string"]]],((Fay$$fayToJs(["user","JSDOM",[]],$p1)).id)));});};JS.onClick = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],((Fay$$fayToJs(["user","JSDOM",[]],$p1)).onclick = (Fay$$fayToJs(["action",[["unknown"]]],$p2)))));});};};JS.byClass = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["list",[["user","JSDOM",[]]]],(document.getElementsByClassName(Fay$$fayToJs_string($p1)))));});};JS.onKeyPress$39$ = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],document.onkeypress = (Fay$$fayToJs(["function",[["user","JS",[]],["action",[["unknown"]]]]],$p1))));});};JS.keyCode = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","JSCharCode",[]],((Fay$$fayToJs(["user","JS",[]],$p1)).keyCode));});};JS.fromCharCode = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_string((String.fromCharCode(Fay$$fayToJs(["user","JSCharCode",[]],$p1))));});};JS.onKeyPress = function($p1){return new Fay$$$(function(){var p = $p1;return Fay$$_(JS.onKeyPress$39$)(Fay$$_(Fay$$_(Prelude.$46$)(p))(Fay$$_(Fay$$_(Prelude.$46$)(JS.fromCharCode))(JS.keyCode)));});};JS.initVars = new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],fayVars = {}));});JS.setVar = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],fayVars[Fay$$fayToJs_string($p1)] = Fay$$fayToJs_string($p2)));});};};JS.getVar$39$ = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["defined",[["string"]]],(fayVars[Fay$$fayToJs_string($p1)])));});};JS.getVar = function($p1){return new Fay$$$(function(){var k = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(JS.getVar$39$)(k)))(function($p1){var o = $p1;return (function($tmp1){if (Fay$$_($tmp1) instanceof Fay.FFI._Undefined) {return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.error))(Fay$$_(Fay$$_(Prelude.$43$$43$)(Fay$$list("unset variable ‘")))(Fay$$_(Fay$$_(Prelude.$43$$43$)(k))(Fay$$list("’"))));}if (Fay$$_($tmp1) instanceof Fay.FFI._Defined) {var v = Fay$$_($tmp1).slot1;return Fay$$_(Fay$$$_return)(v);}return (function(){ throw (["unhandled case",$tmp1]); })();})(o);});});};var Util = {};Util.myintersperse = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {var sep = $p1;return null;}if (Fay$$listLen(Fay$$_($p2),1)) {var a = Fay$$index(0,Fay$$_($p2));var sep = $p1;return Fay$$list([a]);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var cs = $tmp1.cdr;var sep = $p1;return Fay$$_(Fay$$_(Fay$$cons)(a))(Fay$$_(Fay$$_(Fay$$cons)(sep))(Fay$$_(Fay$$_(Util.myintersperse)(sep))(cs)));}throw ["unhandled case in myintersperse",[$p1,$p2]];});};};Util.one = function($p1){return new Fay$$$(function(){var n = $p1;return Fay$$list([n]);});};Util.mymapM = function($p1){return function($p2){return new Fay$$$(function(){var l = $p2;var f = $p1;return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.sequence))(Fay$$_(Fay$$_(Prelude.map)(f))(l));});};};Util.iter = function($p1){return function($p2){return new Fay$$$(function(){var l = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Util.mymapM)(f))(l)))(Fay$$_(Fay$$$_return)(Fay$$unit));});};};Util.mapi = function($p1){return function($p2){return new Fay$$$(function(){var l = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.zipWith)(f))(Prelude.enumFrom(0)))(l);});};};Util.prefix = function($p1){return function($p2){return new Fay$$$(function(){var l = $p2;if (Fay$$_($p1) === null) {return true;}if (Fay$$_($p2) === null) {var l = $p1;return false;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(a))(b)) ? Fay$$_(Fay$$_(Util.prefix)(as))(bs) : false;}}throw ["unhandled case in prefix",[$p1,$p2]];});};};Util.comma = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude.$46$)(Prelude.concat))(Fay$$_(Util.myintersperse)(Fay$$list(",")));});Util.uncomma1 = function($p1){return new Fay$$$(function(){if (Fay$$equal($p1,Fay$$list(""))) {return Prelude.Nothing;}var cs = $p1;return (function(){var r = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {var acc = $p1;return Fay$$list([Fay$$_(Prelude.reverse)(acc),Fay$$list("")]);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$_($tmp1.car) === ",") {var cs = $tmp1.cdr;var acc = $p1;return Fay$$list([Fay$$_(Prelude.reverse)(acc),cs]);}}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var acc = $p1;return Fay$$_(Fay$$_(r)(Fay$$_(Fay$$_(Fay$$cons)(c))(acc)))(cs);}throw ["unhandled case in r",[$p1,$p2]];});};};return Fay$$_(Prelude.Just)(Fay$$_(Fay$$_(r)(null))(cs));})();});};Util.uncomma = function($p1){return new Fay$$$(function(){var s = $p1;return (function(){var r = function($p1){return function($p2){return new Fay$$$(function(){var s = $p2;var acc = $p1;return (function($tmp1){if (Fay$$_($tmp1) instanceof Prelude._Nothing) {return Fay$$_(Prelude.reverse)(acc);}if (Fay$$_($tmp1) instanceof Prelude._Just) {if (Fay$$listLen(Fay$$_(Fay$$_($tmp1).slot1),2)) {var s = Fay$$index(0,Fay$$_(Fay$$_($tmp1).slot1));var ss = Fay$$index(1,Fay$$_(Fay$$_($tmp1).slot1));return Fay$$_(Fay$$_(r)(Fay$$_(Fay$$_(Fay$$cons)(s))(acc)))(ss);}}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Util.uncomma1)(s));});};};return Fay$$_(Fay$$_(r)(null))(s);})();});};Util.digit = function($p1){return new Fay$$$(function(){var c = $p1;return (function($tmp1){if (Fay$$_($tmp1) === "0") {return Fay$$_(Prelude.Just)(0);}if (Fay$$_($tmp1) === "1") {return Fay$$_(Prelude.Just)(1);}if (Fay$$_($tmp1) === "2") {return Fay$$_(Prelude.Just)(2);}if (Fay$$_($tmp1) === "3") {return Fay$$_(Prelude.Just)(3);}if (Fay$$_($tmp1) === "4") {return Fay$$_(Prelude.Just)(4);}if (Fay$$_($tmp1) === "5") {return Fay$$_(Prelude.Just)(5);}if (Fay$$_($tmp1) === "6") {return Fay$$_(Prelude.Just)(6);}if (Fay$$_($tmp1) === "7") {return Fay$$_(Prelude.Just)(7);}if (Fay$$_($tmp1) === "8") {return Fay$$_(Prelude.Just)(8);}if (Fay$$_($tmp1) === "9") {return Fay$$_(Prelude.Just)(9);}return Prelude.Nothing;})(c);});};Util.parseInt$39$ = function($p1){return new Fay$$$(function(){var s = $p1;return (function($tmp1){if (Fay$$_($tmp1) instanceof Prelude._Nothing) {return Fay$$_(Prelude.error)(Fay$$list("parseInt failed"));}if (Fay$$_($tmp1) instanceof Prelude._Just) {var i = Fay$$_($tmp1).slot1;return i;}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Util.parseInt)(s));});};Util.parseInt = function($p1){return new Fay$$$(function(){var l = $p1;return (function(){var r = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var m = $p2;var a = $p1;return Fay$$_(Prelude.Just)(a);}var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var m = $p2;var a = $p1;return (function($tmp1){if (Fay$$_($tmp1) instanceof Prelude._Nothing) {return Prelude.Nothing;}if (Fay$$_($tmp1) instanceof Prelude._Just) {var c = Fay$$_($tmp1).slot1;return Fay$$_(Fay$$_(Fay$$_(r)(Fay$$_(Fay$$_(Fay$$add)(a))(Fay$$_(Fay$$_(Fay$$mult)(m))(c))))(Fay$$_(Fay$$_(Fay$$mult)(m))(10)))(cs);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Util.digit)(c));}throw ["unhandled case in r",[$p1,$p2,$p3]];});};};};return (function($tmp1){if (Fay$$_($tmp1) === null) {return Prelude.Nothing;}var l = $tmp1;return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Fay$$_(r)(0))(1)))(Fay$$_(Prelude.reverse)(l));})(l);})();});};var OL = {};OL._Addr = function Addr(slot1){this.slot1 = slot1;};OL._Addr.prototype.instance = "Addr";OL.Addr = function(slot1){return new Fay$$$(function(){return new OL._Addr(slot1);});};OL._OLStr = function OLStr(slot1){this.slot1 = slot1;};OL._OLStr.prototype.instance = "OLStr";OL.OLStr = function(slot1){return new Fay$$$(function(){return new OL._OLStr(slot1);});};OL._OL = function OL(slot1,slot2){this.slot1 = slot1;this.slot2 = slot2;};OL._OL.prototype.instance = "OL";OL.OL = function(slot1){return function(slot2){return new Fay$$$(function(){return new OL._OL(slot1,slot2);});};};OL._INDENT = function INDENT(){};OL._INDENT.prototype.instance = "INDENT";OL.INDENT = new Fay$$$(function(){return new OL._INDENT();});OL._DEDENT = function DEDENT(){};OL._DEDENT.prototype.instance = "DEDENT";OL.DEDENT = new Fay$$$(function(){return new OL._DEDENT();});OL._LINE = function LINE(slot1){this.slot1 = slot1;};OL._LINE.prototype.instance = "LINE";OL.LINE = function(slot1){return new Fay$$$(function(){return new OL._LINE(slot1);});};OL.trim = function($p1){return new Fay$$$(function(){var s = $p1;return (function(){var unf = function($p1){return new Fay$$$(function(){if (Fay$$equal($p1,Fay$$list(""))) {return Fay$$list("");}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$_($tmp1.car) === " ") {var s = $tmp1.cdr;return Fay$$_(unf)(s);}}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$_($tmp1.car) === "\t") {var s = $tmp1.cdr;return Fay$$_(unf)(s);}}var s = $p1;return s;});};var untab = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === "\t") {return " ";}var x = $p1;return x;});};return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Prelude.map)(untab)))(Fay$$_(Fay$$_(Prelude.$36$)(Prelude.reverse))(Fay$$_(Fay$$_(Prelude.$36$)(unf))(Fay$$_(Fay$$_(Prelude.$36$)(Prelude.reverse))(Fay$$_(unf)(s)))));})();});};OL.ols = function($p1){return new Fay$$$(function(){var s = $p1;return Fay$$_(Fay$$_(Prelude.$36$)(OL.OLStr))((function($tmp1){if (Fay$$_($tmp1) === null) {return Fay$$list("#");}var ts = $tmp1;return ts;})(Fay$$_(OL.trim)(s)));});};OL.unols = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof OL._OLStr) {var s = Fay$$_($p1).slot1;return s;}throw ["unhandled case in unols",[$p1]];});};OL.$945$ = new Fay$$$(function(){return OL.ols;});OL.ol = function($p1){return function($p2){return new Fay$$$(function(){var s = $p2;var t = $p1;return Fay$$_(Fay$$_(OL.OL)(Fay$$_(OL.$945$)(t)))(s);});};};OL.l = function($p1){return new Fay$$$(function(){var t = $p1;return Fay$$_(Fay$$_(OL.OL)(Fay$$_(OL.$945$)(t)))(null);});};OL.olexample = new Fay$$$(function(){return Fay$$_(Fay$$_(OL.ol)(Fay$$list("h")))(Fay$$list([Fay$$_(OL.l)(Fay$$list("i")),Fay$$_(OL.l)(Fay$$list("j")),Fay$$_(Fay$$_(OL.ol)(Fay$$list("k")))(Fay$$list([Fay$$_(OL.l)(Fay$$list("hihihi there")),Fay$$_(Fay$$_(OL.ol)(Fay$$list("w")))(Fay$$list([Fay$$_(OL.l)(Fay$$list("t")),Fay$$_(OL.l)(Fay$$list("f"))]))]))]));});OL.addrmap = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var l = $p3;var f = $p2;if (Fay$$_($p1) instanceof OL._Addr) {var a = Fay$$_($p1).slot1;return Fay$$_(Fay$$_(Util.mapi)(function($p1){var i = $p1;return function($p2){var e = $p2;return Fay$$_(Fay$$_(f)(Fay$$_(OL.Addr)(Fay$$_(Fay$$_(Fay$$cons)(i))(a))))(e);};}))(l);}throw ["unhandled case in addrmap",[$p1,$p2,$p3]];});};};};OL.addrshow = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof OL._Addr) {var a = Fay$$_($p1).slot1;return Fay$$_(Fay$$_(Prelude.$36$)(Util.comma))(Fay$$_(Fay$$_(Prelude.map)(Prelude.show))(a));}throw ["unhandled case in addrshow",[$p1]];});};OL.addrread = function($p1){return new Fay$$$(function(){var s = $p1;return Fay$$_(Fay$$_(Prelude.$36$)(OL.Addr))(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Prelude.map)(Util.parseInt$39$)))(Fay$$_(Util.uncomma)(s)));});};OL.olshow = function($p1){return new Fay$$$(function(){var n = $p1;return (function(){var join = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude.$46$)(Prelude.concat))(Fay$$_(Util.myintersperse)(Fay$$list("\n")));});var r = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) instanceof OL._OL) {if (Fay$$_(Fay$$_($p2).slot1) instanceof OL._OLStr) {var s = Fay$$_(Fay$$_($p2).slot1).slot1;if (Fay$$_(Fay$$_($p2).slot2) === null) {var d = $p1;return Fay$$_(Fay$$_(Prelude.$43$$43$)(Fay$$_(indent)(d)))(s);}var s = Fay$$_(Fay$$_($p2).slot1).slot1;var cs = Fay$$_($p2).slot2;var d = $p1;return Fay$$_(Fay$$_(Prelude.$36$)(join))(Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Prelude.$43$$43$)(Fay$$_(indent)(d)))(s)))(Fay$$_(Fay$$_(Prelude.map)(Fay$$_(r)(Fay$$_(Fay$$_(Fay$$add)(d))(1))))(cs)));}}throw ["unhandled case in r",[$p1,$p2]];});};};var indent = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === 0) {return Fay$$list("");}var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? Fay$$_(Prelude.error)(Fay$$list("Bad Logic in ‘indent’")) : Fay$$_(Fay$$_(Prelude.$43$$43$)(Fay$$list("\t")))(Fay$$_(indent)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)));});};return Fay$$_(Fay$$_(Prelude.$43$$43$)(Fay$$_(Fay$$_(r)(0))(n)))(Fay$$list("\n"));})();});};OL.olread = new Fay$$$(function(){return new Fay$$$(function(){var reorder = function($p1){return new Fay$$$(function(){var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.car;var $tmp2 = Fay$$_($tmp1.cdr);if ($tmp2 instanceof Fay$$Cons) {if (Fay$$_($tmp2.car) instanceof OL._INDENT) {var ts = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$cons)(OL.INDENT))(Fay$$_(Fay$$_(Fay$$cons)(t))(Fay$$_(reorder)(ts)));}}}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.car;var ts = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(t))(Fay$$_(reorder)(ts));}if (Fay$$_($p1) === null) {return null;}throw ["unhandled case in reorder",[$p1]];});};var finalize = function($p1){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p1),2)) {var ol = Fay$$index(0,Fay$$_($p1));if (Fay$$_(Fay$$index(1,Fay$$_($p1))) === null) {return ol;}}return Fay$$_(Prelude.error)(Fay$$list("wut.finalize"));});};var ppp = function($p1){return new Fay$$$(function(){var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$_($tmp1.car) instanceof OL._INDENT) {var $tmp2 = Fay$$_($tmp1.cdr);if ($tmp2 instanceof Fay$$Cons) {if (Fay$$_($tmp2.car) instanceof OL._LINE) {var l = Fay$$_($tmp2.car).slot1;var ts = $tmp2.cdr;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var subs = Fay$$index(0,Fay$$_($tmp1));var r = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(OL.OL)(l))(subs),r]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(pseq)(null))(ts));}}}}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$_($tmp1.car) instanceof OL._LINE) {var l = Fay$$_($tmp1.car).slot1;var ts = $tmp1.cdr;return Fay$$list([Fay$$_(Fay$$_(OL.OL)(l))(null),ts]);}}return Fay$$_(Prelude.error)(Fay$$list("wut.ppp"));});};var pseq = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$_($tmp1.car) instanceof OL._DEDENT) {var remain = $tmp1.cdr;var acc = $p1;return Fay$$list([Fay$$_(Prelude.reverse)(acc),remain]);}}var ts = $p2;var acc = $p1;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var t = Fay$$index(0,Fay$$_($tmp1));var remain = Fay$$index(1,Fay$$_($tmp1));return Fay$$_(Fay$$_(pseq)(Fay$$_(Fay$$_(Fay$$cons)(t))(acc)))(remain);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(ppp)(ts));});};};var ollex = function($p1){return new Fay$$$(function(){var s = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$_(getIndent)(null))(0))(0))(s);});};var getIndent = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){var $tmp1 = Fay$$_($p4);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$_($tmp1.car) === "\t") {var cs = $tmp1.cdr;var count = $p3;var pr = $p2;var acc = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$_(getIndent)(acc))(pr))(Fay$$_(Fay$$_(Fay$$add)(count))(1)))(cs);}}var cs = $p4;var count = $p3;var pr = $p2;var acc = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$_(dent)(acc))(pr))(count))(cs);});};};};};var pgetText = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){if (Fay$$_($p4) === null) {if (Fay$$_($p3) === null) {var pr = $p2;var acc = $p1;return Fay$$_(Prelude.reverse)(acc);}}if (Fay$$equal($p4,Fay$$list("\n"))) {if (Fay$$_($p3) === null) {var pr = $p2;var acc = $p1;return Fay$$_(Prelude.reverse)(acc);}}if (Fay$$_($p4) === null) {var str = $p3;var pr = $p2;var acc = $p1;return Fay$$_(Prelude.reverse)(Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Prelude.$36$)(OL.LINE))(Fay$$_(Fay$$_(Prelude.$36$)(OL.ols))(Fay$$_(Prelude.reverse)(str)))))(acc));}var $tmp1 = Fay$$_($p4);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$_($tmp1.car) === "\n") {var cs = $tmp1.cdr;var str = $p3;var pr = $p2;var acc = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$_(getIndent)(Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Prelude.$36$)(OL.LINE))(Fay$$_(Fay$$_(Prelude.$36$)(OL.ols))(Fay$$_(Prelude.reverse)(str)))))(acc)))(pr))(0))(cs);}}var $tmp1 = Fay$$_($p4);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var str = $p3;var pr = $p2;var acc = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$_(pgetText)(acc))(pr))(Fay$$_(Fay$$_(Fay$$cons)(c))(str)))(cs);}throw ["unhandled case in pgetText",[$p1,$p2,$p3,$p4]];});};};};};var overdent = new Fay$$$(function(){return Fay$$list("too much indentation");});var dent = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){var cs = $p4;var n = $p3;var o = $p2;var acc = $p1;return (function($tmp1){if (Fay$$_($tmp1) instanceof Prelude._GT) {return Fay$$_(Fay$$_(Fay$$_(Fay$$neq)(Fay$$_(Fay$$_(Fay$$add)(o))(1)))(n)) ? Fay$$_(Prelude.error)(overdent) : Fay$$_(Fay$$_(Fay$$_(Fay$$_(pgetText)(Fay$$_(Fay$$_(Fay$$cons)(OL.INDENT))(acc)))(n))(Fay$$list("")))(cs);}if (Fay$$_($tmp1) instanceof Prelude._LT) {return Fay$$_(Fay$$_(Fay$$_(Fay$$_(dent)(Fay$$_(Fay$$_(Fay$$cons)(OL.DEDENT))(acc)))(Fay$$_(Fay$$_(Fay$$sub)(o))(1)))(n))(cs);}if (Fay$$_($tmp1) instanceof Prelude._EQ) {return Fay$$_(Fay$$_(Fay$$_(Fay$$_(pgetText)(acc))(n))(Fay$$list("")))(cs);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.compare)(n))(o));});};};};};return Fay$$_(Fay$$_(Prelude.$46$)(finalize))(Fay$$_(Fay$$_(Prelude.$46$)(ppp))(Fay$$_(Fay$$_(Prelude.$46$)(reorder))(ollex)));});});OL.olmap = function($p1){return function($p2){return new Fay$$$(function(){var ol = $p2;var f = $p1;return (function(){var descend = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) instanceof OL._OL) {var l = Fay$$_($p2).slot1;if (Fay$$_(Fay$$_($p2).slot2) === null) {var f = $p1;return Fay$$_(Fay$$_(OL.OL)(l))(null);}var l = Fay$$_($p2).slot1;var subs = Fay$$_($p2).slot2;var f = $p1;return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(OL.OL)(l)))(Fay$$_(Fay$$_(Prelude.map)(Fay$$_(OL.olmap)(f)))(subs));}throw ["unhandled case in descend",[$p1,$p2]];});};};return (function($tmp1){if (Fay$$_($tmp1) instanceof Prelude._Just) {var ol$39$ = Fay$$_($tmp1).slot1;return ol$39$;}if (Fay$$_($tmp1) instanceof Prelude._Nothing) {return Fay$$_(Fay$$_(descend)(f))(ol);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(f)(ol));})();});};};OL.olmapAddr = function($p1){return function($p2){return new Fay$$$(function(){var ol = $p2;var f = $p1;return (function(){var olmapAddr$39$ = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var ol = $p3;var a = $p2;var f = $p1;return (function($tmp1){if (Fay$$_($tmp1) instanceof Prelude._Just) {var ol$39$ = Fay$$_($tmp1).slot1;return ol$39$;}if (Fay$$_($tmp1) instanceof Prelude._Nothing) {return Fay$$_(Fay$$_(Fay$$_(descendAddr)(f))(a))(ol);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(f)(a))(ol));});};};};var descendAddr = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) instanceof OL._OL) {var l = Fay$$_($p3).slot1;if (Fay$$_(Fay$$_($p3).slot2) === null) {var addr = $p2;var f = $p1;return Fay$$_(Fay$$_(OL.OL)(l))(null);}var l = Fay$$_($p3).slot1;var subs = Fay$$_($p3).slot2;var addr = $p2;var f = $p1;return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(OL.OL)(l)))(Fay$$_(Fay$$_(Fay$$_(OL.addrmap)(addr))(Fay$$_(olmapAddr$39$)(f)))(subs));}throw ["unhandled case in descendAddr",[$p1,$p2,$p3]];});};};};var ns = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof OL._OL) {var s = Fay$$_($p1).slot1;if (Fay$$_(Fay$$_($p1).slot2) === null) {return s;}var s = Fay$$_($p1).slot1;return s;}throw ["unhandled case in ns",[$p1]];});};return Fay$$_(Fay$$_(Fay$$_(olmapAddr$39$)(f))(Fay$$_(OL.Addr)(null)))(ol);})();});};};OL.isChildOf = function($p1){return function($p2){return new Fay$$$(function(){var b = $p2;var a = $p1;return (function(){var arr = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Prelude.Nothing;}if (Fay$$_($p2) === null) {if (Fay$$listLen(Fay$$_($p1),1)) {var i = Fay$$index(0,Fay$$_($p1));return Fay$$_(Prelude.Just)(i);}return Prelude.Nothing;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(a))(b)) ? Fay$$_(Fay$$_(arr)(as))(bs) : Prelude.Nothing;}}throw ["unhandled case in arr",[$p1,$p2]];});};};return Fay$$_(Fay$$_(arr)(Fay$$_(Prelude.reverse)(a)))(Fay$$_(Prelude.reverse)(b));})();});};};Fay$$objConcat(Fay$$fayToJsHash,{"Addr": function(type,argTypes,_obj){var obj_ = {"instance": "Addr"};var obj_slot1 = Fay$$fayToJs(["list",[["int"]]],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OLStr": function(type,argTypes,_obj){var obj_ = {"instance": "OLStr"};var obj_slot1 = Fay$$fayToJs_string(_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OL": function(type,argTypes,_obj){var obj_ = {"instance": "OL"};var obj_slot1 = Fay$$fayToJs(["user","OLStr",[]],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}var obj_slot2 = Fay$$fayToJs(["list",[["user","OL",[]]]],_obj.slot2);if (undefined !== obj_slot2) {obj_['slot2'] = obj_slot2;}return obj_;},"INDENT": function(type,argTypes,_obj){var obj_ = {"instance": "INDENT"};return obj_;},"DEDENT": function(type,argTypes,_obj){var obj_ = {"instance": "DEDENT"};return obj_;},"LINE": function(type,argTypes,_obj){var obj_ = {"instance": "LINE"};var obj_slot1 = Fay$$fayToJs(["user","OLStr",[]],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}});Fay$$objConcat(Fay$$jsToFayHash,{"Addr": function(type,argTypes,obj){return new OL._Addr(Fay$$jsToFay(["list",[["int"]]],obj["slot1"]));},"OLStr": function(type,argTypes,obj){return new OL._OLStr(Fay$$jsToFay_string(obj["slot1"]));},"OL": function(type,argTypes,obj){return new OL._OL(Fay$$jsToFay(["user","OLStr",[]],obj["slot1"]),Fay$$jsToFay(["list",[["user","OL",[]]]],obj["slot2"]));},"INDENT": function(type,argTypes,obj){return new OL._INDENT();},"DEDENT": function(type,argTypes,obj){return new OL._DEDENT();},"LINE": function(type,argTypes,obj){return new OL._LINE(Fay$$jsToFay(["user","OLStr",[]],obj["slot1"]));}});var DOM = {};DOM._Node = function Node(slot1,slot2,slot3,slot4){this.slot1 = slot1;this.slot2 = slot2;this.slot3 = slot3;this.slot4 = slot4;};DOM._Node.prototype.instance = "Node";DOM.Node = function(slot1){return function(slot2){return function(slot3){return function(slot4){return new Fay$$$(function(){return new DOM._Node(slot1,slot2,slot3,slot4);});};};};};DOM.addrId = function($p1){return new Fay$$$(function(){var a = $p1;return Fay$$_(Fay$$_(Prelude.$43$$43$)(Fay$$list("olnode")))(Fay$$_(OL.addrshow)(a));});};DOM.idAddr = function($p1){return new Fay$$$(function(){var s = $p1;return Fay$$_(Fay$$_(Prelude.not)(Fay$$_(Fay$$_(Util.prefix)(Fay$$list("olnode")))(s))) ? Fay$$_(Prelude.error)(Fay$$list("bad id")) : Fay$$_(OL.addrread)(Fay$$_(Fay$$_(Prelude.drop)(6))(s));});};DOM.olDom = function($p1){return function($p2){return new Fay$$$(function(){var tree = $p2;var selection = $p1;return (function(){var $945$ = new Fay$$$(function(){return OL.unols;});var leaf = function($p1){return function($p2){return new Fay$$$(function(){var txt = $p2;var a = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$_(DOM.Node)(Fay$$list("span")))(Fay$$_(attrs)(a)))(Fay$$_(Prelude.Just)(txt)))(null);});};};var attrs = function($p1){return new Fay$$$(function(){var a = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(a))(selection)) ? Fay$$list([Fay$$list([Fay$$list("class"),Fay$$list("selected")]),Fay$$list([Fay$$list("id"),Fay$$_(DOM.addrId)(a)])]) : Fay$$list([Fay$$list([Fay$$list("class"),Fay$$list("unselected")]),Fay$$list([Fay$$list("id"),Fay$$_(DOM.addrId)(a)])]);});};var top = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof OL._OL) {var txt = Fay$$_($p1).slot1;if (Fay$$_(Fay$$_($p1).slot2) === null) {return Fay$$_(Fay$$_(Fay$$_(Fay$$_(DOM.Node)(Fay$$list("p")))(null))(Prelude.Nothing))(Fay$$list([Fay$$_(Fay$$_(leaf)(Fay$$_(OL.Addr)(null)))(Fay$$_($945$)(txt))]));}var txt = Fay$$_($p1).slot1;var cs = Fay$$_($p1).slot2;return Fay$$_(Fay$$_(Fay$$_(Fay$$_(DOM.Node)(Fay$$list("p")))(null))(Prelude.Nothing))(Fay$$list([Fay$$_(Fay$$_(leaf)(Fay$$_(OL.Addr)(null)))(Fay$$_($945$)(txt)),Fay$$_(Fay$$_(Fay$$_(Fay$$_(DOM.Node)(Fay$$list("ul")))(null))(Prelude.Nothing))(Fay$$_(Fay$$_(Prelude.$36$)(Prelude.concat))(Fay$$_(Fay$$_(Fay$$_(OL.addrmap)(Fay$$_(OL.Addr)(null)))(r))(cs)))]));}throw ["unhandled case in top",[$p1]];});};var r = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) instanceof OL._OL) {var txt = Fay$$_($p2).slot1;if (Fay$$_(Fay$$_($p2).slot2) === null) {var a = $p1;return Fay$$list([Fay$$_(Fay$$_(Fay$$_(Fay$$_(DOM.Node)(Fay$$list("li")))(null))(Prelude.Nothing))(Fay$$list([Fay$$_(Fay$$_(leaf)(a))(Fay$$_($945$)(txt))]))]);}var txt = Fay$$_($p2).slot1;var cs = Fay$$_($p2).slot2;var a = $p1;return Fay$$list([Fay$$_(Fay$$_(Fay$$_(Fay$$_(DOM.Node)(Fay$$list("li")))(null))(Prelude.Nothing))(Fay$$list([Fay$$_(Fay$$_(leaf)(a))(Fay$$_($945$)(txt))])),Fay$$_(Fay$$_(Fay$$_(Fay$$_(DOM.Node)(Fay$$list("ul")))(null))(Prelude.Nothing))(Fay$$_(Fay$$_(Prelude.$36$)(Prelude.concat))(Fay$$_(Fay$$_(Fay$$_(OL.addrmap)(a))(r))(cs)))]);}throw ["unhandled case in r",[$p1,$p2]];});};};return Fay$$_(top)(tree);})();});};};Fay$$objConcat(Fay$$fayToJsHash,{"Node": function(type,argTypes,_obj){var obj_ = {"instance": "Node"};var obj_slot1 = Fay$$fayToJs_string(_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}var obj_slot2 = Fay$$fayToJs(["list",[["tuple",[["string"],["string"]]]]],_obj.slot2);if (undefined !== obj_slot2) {obj_['slot2'] = obj_slot2;}var obj_slot3 = Fay$$fayToJs(["user","Maybe",[["string"]]],_obj.slot3);if (undefined !== obj_slot3) {obj_['slot3'] = obj_slot3;}var obj_slot4 = Fay$$fayToJs(["list",[["user","DOM",[]]]],_obj.slot4);if (undefined !== obj_slot4) {obj_['slot4'] = obj_slot4;}return obj_;}});Fay$$objConcat(Fay$$jsToFayHash,{"Node": function(type,argTypes,obj){return new DOM._Node(Fay$$jsToFay_string(obj["slot1"]),Fay$$jsToFay(["list",[["tuple",[["string"],["string"]]]]],obj["slot2"]),Fay$$jsToFay(["user","Maybe",[["string"]]],obj["slot3"]),Fay$$jsToFay(["list",[["user","DOM",[]]]],obj["slot4"]));}});var Edit = {};Edit._State = function State(slot1,slot2){this.slot1 = slot1;this.slot2 = slot2;};Edit._State.prototype.instance = "State";Edit.State = function(slot1){return function(slot2){return new Fay$$$(function(){return new Edit._State(slot1,slot2);});};};Edit._SelDown = function SelDown(){};Edit._SelDown.prototype.instance = "SelDown";Edit.SelDown = new Fay$$$(function(){return new Edit._SelDown();});Edit._SelLeft = function SelLeft(){};Edit._SelLeft.prototype.instance = "SelLeft";Edit.SelLeft = new Fay$$$(function(){return new Edit._SelLeft();});Edit._SelUp = function SelUp(){};Edit._SelUp.prototype.instance = "SelUp";Edit.SelUp = new Fay$$$(function(){return new Edit._SelUp();});Edit._SelRight = function SelRight(){};Edit._SelRight.prototype.instance = "SelRight";Edit.SelRight = new Fay$$$(function(){return new Edit._SelRight();});Edit._Select = function Select(slot1){this.slot1 = slot1;};Edit._Select.prototype.instance = "Select";Edit.Select = function(slot1){return new Fay$$$(function(){return new Edit._Select(slot1);});};Edit._Edit = function Edit(slot1){this.slot1 = slot1;};Edit._Edit.prototype.instance = "Edit";Edit.Edit = function(slot1){return new Fay$$$(function(){return new Edit._Edit(slot1);});};Edit._Delete = function Delete(){};Edit._Delete.prototype.instance = "Delete";Edit.Delete = new Fay$$$(function(){return new Edit._Delete();});Edit._Nada = function Nada(){};Edit._Nada.prototype.instance = "Nada";Edit.Nada = new Fay$$$(function(){return new Edit._Nada();});Edit._InsBefore = function InsBefore(slot1){this.slot1 = slot1;};Edit._InsBefore.prototype.instance = "InsBefore";Edit.InsBefore = function(slot1){return new Fay$$$(function(){return new Edit._InsBefore(slot1);});};Edit._InsAfter = function InsAfter(slot1){this.slot1 = slot1;};Edit._InsAfter.prototype.instance = "InsAfter";Edit.InsAfter = function(slot1){return new Fay$$$(function(){return new Edit._InsAfter(slot1);});};Edit._InsAbove = function InsAbove(slot1){this.slot1 = slot1;};Edit._InsAbove.prototype.instance = "InsAbove";Edit.InsAbove = function(slot1){return new Fay$$$(function(){return new Edit._InsAbove(slot1);});};Edit._InsBelow = function InsBelow(slot1){this.slot1 = slot1;};Edit._InsBelow.prototype.instance = "InsBelow";Edit.InsBelow = function(slot1){return new Fay$$$(function(){return new Edit._InsBelow(slot1);});};Edit.fudgeAddr = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof Edit._State) {if (Fay$$_(Fay$$_($p1).slot1) instanceof OL._Addr) {var a = Fay$$_(Fay$$_($p1).slot1).slot1;var ol = Fay$$_($p1).slot2;return (function(){var fudge = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) instanceof OL._OL) {var s = Fay$$_($p2).slot1;if (Fay$$_($p1) === null) {return null;}if (Fay$$_(Fay$$_($p2).slot2) === null) {return null;}var s = Fay$$_($p2).slot1;var sub = Fay$$_($p2).slot2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(a))(0)) ? Fay$$_(Prelude.error)(Fay$$list("there is a bug in fudgeAddr.")) : Fay$$_(Fay$$_(Fay$$_(Fay$$gte)(a))(Fay$$_(Prelude.length)(sub))) ? Fay$$_(Fay$$_(fudge)(Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Fay$$sub)(a))(1)))(as)))(Fay$$_(Fay$$_(OL.OL)(s))(sub)) : Fay$$_(Fay$$_(Fay$$cons)(a))(Fay$$_(Fay$$_(fudge)(as))(Fay$$_(Fay$$_(Prelude.$33$$33$)(sub))(a)));}}throw ["unhandled case in fudge",[$p1,$p2]];});};};return Fay$$_(Fay$$_(Edit.State)(Fay$$_(Fay$$_(Prelude.$36$)(OL.Addr))(Fay$$_(Fay$$_(Prelude.$36$)(Prelude.reverse))(Fay$$_(Fay$$_(fudge)(Fay$$_(Prelude.reverse)(a)))(ol)))))(ol);})();}}throw ["unhandled case in fudgeAddr",[$p1]];});};Edit.getNode = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof Edit._State) {if (Fay$$_(Fay$$_($p1).slot1) instanceof OL._Addr) {var a = Fay$$_(Fay$$_($p1).slot1).slot1;var ol = Fay$$_($p1).slot2;return (function(){var r = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) instanceof OL._OL) {var s = Fay$$_($p2).slot1;if (Fay$$_($p1) === null) {return s;}if (Fay$$_(Fay$$_($p2).slot2) === null) {return Fay$$_(Prelude.error)(Fay$$list("invalid selection"));}var sub = Fay$$_($p2).slot2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return Fay$$_(Fay$$_(Prelude.or)(Fay$$list([Fay$$_(Fay$$_(Fay$$gte)(a))(Fay$$_(Prelude.length)(sub)),Fay$$_(Fay$$_(Fay$$lt)(a))(0)]))) ? Fay$$_(Prelude.error)(Fay$$list("invalid selection")) : Fay$$_(Fay$$_(r)(as))(Fay$$_(Fay$$_(Prelude.$33$$33$)(sub))(a));}}throw ["unhandled case in r",[$p1,$p2]];});};};return Fay$$_(Fay$$_(r)(Fay$$_(Prelude.reverse)(a)))(ol);})();}}throw ["unhandled case in getNode",[$p1]];});};Edit.olinsertAt = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var n = $p3;var txt = $p2;if (Fay$$_($p1) instanceof OL._Addr) {var addr = Fay$$_($p1).slot1;return (function(){var f = function($p1){return function($p2){return new Fay$$$(function(){var n = $p2;if (Fay$$_($p1) instanceof OL._Addr) {var a = Fay$$_($p1).slot1;return (function($tmp1){if (Fay$$_($tmp1) instanceof Prelude._Nothing) {return Prelude.Nothing;}if (Fay$$_($tmp1) instanceof Prelude._Just) {var index = Fay$$_($tmp1).slot1;return (function($tmp2){if (Fay$$_($tmp2) instanceof OL._OL) {if (Fay$$_(Fay$$_($tmp2).slot2) === null) {return Fay$$_(Prelude.error)(Fay$$list("Bad address given to olinsertAt"));}var s = Fay$$_($tmp2).slot1;var cs = Fay$$_($tmp2).slot2;return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.Just))(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(OL.OL)(s)))(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Fay$$_(Edit.insertAt)(cs))(index)))(Fay$$_(Fay$$_(OL.OL)(txt))(null))));}return (function(){ throw (["unhandled case",$tmp2]); })();})(n);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(OL.isChildOf)(addr))(a));}throw ["unhandled case in f",[$p1,$p2]];});};};return Fay$$_(Fay$$_(OL.olmapAddr)(f))(n);})();}throw ["unhandled case in olinsertAt",[$p1,$p2,$p3]];});};};};Edit.insertAt = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var e = $p3;if (Fay$$_($p2) === 0) {var l = $p1;return Fay$$_(Fay$$_(Fay$$cons)(e))(l);}var e = $p3;var n = $p2;if (Fay$$_($p1) === null) {return Fay$$list([e]);}var e = $p3;var n = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(a))(Fay$$_(Fay$$_(Fay$$_(Edit.insertAt)(as))(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(e));}throw ["unhandled case in insertAt",[$p1,$p2,$p3]];});};};};Edit.validSel = function($p1){return function($p2){return new Fay$$$(function(){var ol = $p2;if (Fay$$_($p1) instanceof OL._Addr) {var a = Fay$$_($p1).slot1;return (function(){var r = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return true;}if (Fay$$_($p2) instanceof OL._OL) {if (Fay$$_(Fay$$_($p2).slot2) === null) {return false;}var sub = Fay$$_($p2).slot2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return Fay$$_(Fay$$_(Prelude.or)(Fay$$list([Fay$$_(Fay$$_(Fay$$gte)(a))(Fay$$_(Prelude.length)(sub)),Fay$$_(Fay$$_(Fay$$lt)(a))(0)]))) ? false : Fay$$_(Fay$$_(r)(as))(Fay$$_(Fay$$_(Prelude.$33$$33$)(sub))(a));}}throw ["unhandled case in r",[$p1,$p2]];});};};return Fay$$_(Fay$$_(r)(Fay$$_(Prelude.reverse)(a)))(ol);})();}throw ["unhandled case in validSel",[$p1,$p2]];});};};Edit.down = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof OL._Addr) {var a = Fay$$_($p1).slot1;return Fay$$_(Fay$$_(Prelude.$36$)(OL.Addr))((function($tmp1){if (Fay$$_($tmp1) === null) {return null;}var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var b = $tmp2.car;var bs = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Fay$$add)(b))(1)))(bs);}return (function(){ throw (["unhandled case",$tmp1]); })();})(a));}throw ["unhandled case in down",[$p1]];});};Edit.left = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof OL._Addr) {var a = Fay$$_($p1).slot1;return Fay$$_(Fay$$_(Prelude.$36$)(OL.Addr))((function($tmp1){if (Fay$$_($tmp1) === null) {return null;}var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var b = $tmp2.car;var bs = $tmp2.cdr;return bs;}return (function(){ throw (["unhandled case",$tmp1]); })();})(a));}throw ["unhandled case in left",[$p1]];});};Edit.right = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof OL._Addr) {var a = Fay$$_($p1).slot1;return Fay$$_(Fay$$_(Prelude.$36$)(OL.Addr))(Fay$$_(Fay$$_(Fay$$cons)(0))(a));}throw ["unhandled case in right",[$p1]];});};Edit.up = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof OL._Addr) {var a = Fay$$_($p1).slot1;return Fay$$_(Fay$$_(Prelude.$36$)(OL.Addr))((function($tmp1){if (Fay$$_($tmp1) === null) {return null;}var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var b = $tmp2.car;var bs = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Fay$$sub)(b))(1)))(bs);}return (function(){ throw (["unhandled case",$tmp1]); })();})(a));}throw ["unhandled case in up",[$p1]];});};Edit.moveTo = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) instanceof Edit._State) {var a = Fay$$_($p2).slot1;var o = Fay$$_($p2).slot2;var a$39$ = $p1;return Fay$$_(Fay$$_(Fay$$_(Edit.validSel)(a$39$))(o)) ? Fay$$_(Fay$$_(Edit.State)(a$39$))(o) : Fay$$_(Fay$$_(Edit.State)(a))(o);}throw ["unhandled case in moveTo",[$p1,$p2]];});};};Edit.olreplace = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var n = $p3;var txt = $p2;var addr = $p1;return (function(){var f = function($p1){return function($p2){return new Fay$$$(function(){var n = $p2;var a = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$neq)(a))(addr)) ? Prelude.Nothing : (function($tmp1){if (Fay$$_($tmp1) instanceof OL._OL) {if (Fay$$_(Fay$$_($tmp1).slot2) === null) {return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.Just))(Fay$$_(Fay$$_(OL.OL)(txt))(null));}var subs = Fay$$_($tmp1).slot2;return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.Just))(Fay$$_(Fay$$_(OL.OL)(txt))(subs));}return (function(){ throw (["unhandled case",$tmp1]); })();})(n);});};};return Fay$$_(Fay$$_(OL.olmapAddr)(f))(n);})();});};};};Edit.olreplaceAt = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var o = $p3;var f = $p2;if (Fay$$_($p1) instanceof OL._Addr) {if (Fay$$_(Fay$$_($p1).slot1) === null) {return (function($tmp1){if (Fay$$_($tmp1) instanceof Prelude._Nothing) {return Fay$$_(Fay$$_(OL.OL)(Fay$$_(OL.ols)(Fay$$list("#"))))(null);}if (Fay$$_($tmp1) instanceof Prelude._Just) {var o$39$ = Fay$$_($tmp1).slot1;return o$39$;}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(f)(o));}}var o = $p3;var f = $p2;if (Fay$$_($p1) instanceof OL._Addr) {var $tmp1 = Fay$$_(Fay$$_($p1).slot1);if ($tmp1 instanceof Fay$$Cons) {var i = $tmp1.car;var parent = $tmp1.cdr;return (function(){var r = function($p1){return function($p2){return new Fay$$$(function(){var n = $p2;if (Fay$$_($p1) instanceof OL._Addr) {var a = Fay$$_($p1).slot1;return Fay$$_(Fay$$_(Fay$$_(Fay$$neq)(a))(parent)) ? Prelude.Nothing : (function($tmp1){if (Fay$$_($tmp1) instanceof OL._OL) {if (Fay$$_(Fay$$_($tmp1).slot2) === null) {return Prelude.Nothing;}var t = Fay$$_($tmp1).slot1;var subs = Fay$$_($tmp1).slot2;return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.Just))(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(OL.OL)(t)))(Fay$$_(Fay$$_(Fay$$_(Edit.replaceAt)(subs))(i))(f)));}return (function(){ throw (["unhandled case",$tmp1]); })();})(n);}throw ["unhandled case in r",[$p1,$p2]];});};};return Fay$$_(Fay$$_(OL.olmapAddr)(r))(o);})();}}throw ["unhandled case in olreplaceAt",[$p1,$p2,$p3]];});};};};Edit.replaceAt = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var f = $p3;var n = $p2;if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("replaceAt: Bad list index"));}var f = $p3;if (Fay$$_($p2) === 0) {var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return (function($tmp1){if (Fay$$_($tmp1) instanceof Prelude._Just) {var a$39$ = Fay$$_($tmp1).slot1;return Fay$$_(Fay$$_(Fay$$cons)(a$39$))(as);}if (Fay$$_($tmp1) instanceof Prelude._Nothing) {return as;}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(f)(a));}}var f = $p3;var n = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(a))(Fay$$_(Fay$$_(Fay$$_(Edit.replaceAt)(as))(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(f));}throw ["unhandled case in replaceAt",[$p1,$p2,$p3]];});};};};Edit.insAbove = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var o = $p3;var a = $p2;var t = $p1;return (function(){var r = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.Just))(Fay$$_(Fay$$_(OL.OL)(t))(Fay$$list([x])));});};return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Edit.State)(a)))(Fay$$_(Fay$$_(Fay$$_(Edit.olreplaceAt)(a))(r))(o));})();});};};};Edit.insBelow = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var o = $p3;var a = $p2;var t = $p1;return (function(){var r = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof OL._OL) {var l = Fay$$_($p1).slot1;if (Fay$$_(Fay$$_($p1).slot2) === null) {return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.Just))(Fay$$_(Fay$$_(OL.OL)(l))(Fay$$list([Fay$$_(Fay$$_(OL.OL)(t))(null)])));}var l = Fay$$_($p1).slot1;var cs = Fay$$_($p1).slot2;return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.Just))(Fay$$_(Fay$$_(OL.OL)(l))(Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(OL.OL)(t))(null)))(cs)));}throw ["unhandled case in r",[$p1]];});};return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Edit.apply)(Edit.SelRight)))(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Edit.State)(a)))(Fay$$_(Fay$$_(Fay$$_(Edit.olreplaceAt)(a))(r))(o)));})();});};};};Edit.del = function($p1){return function($p2){return new Fay$$$(function(){var o = $p2;var a = $p1;return Fay$$_(Fay$$_(Prelude.$36$)(Edit.fudgeAddr))(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Edit.State)(a)))(Fay$$_(Fay$$_(Fay$$_(Edit.olreplaceAt)(a))(function($p1){return Prelude.Nothing;}))(o)));});};};Edit.applies = function($p1){return function($p2){return new Fay$$$(function(){var s = $p2;var muts = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.foldl)(Fay$$_(Prelude.flip)(Edit.apply)))(s))(muts);});};};Edit.apply = function($p1){return function($p2){return new Fay$$$(function(){var s = $p2;if (Fay$$_($p2) instanceof Edit._State) {var a = Fay$$_($p2).slot1;var o = Fay$$_($p2).slot2;var op = $p1;return (function($tmp1){if (Fay$$_($tmp1) instanceof Edit._Nada) {return s;}if (Fay$$_($tmp1) instanceof Edit._Delete) {return Fay$$_(Fay$$_(Edit.del)(a))(o);}if (Fay$$_($tmp1) instanceof Edit._Edit) {var t = Fay$$_($tmp1).slot1;return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Edit.State)(a)))(Fay$$_(Fay$$_(Fay$$_(Edit.olreplace)(a))(t))(o));}if (Fay$$_($tmp1) instanceof Edit._Select) {var a$39$ = Fay$$_($tmp1).slot1;return Fay$$_(Fay$$_(Edit.moveTo)(a$39$))(s);}if (Fay$$_($tmp1) instanceof Edit._SelDown) {return Fay$$_(Fay$$_(Edit.moveTo)(Fay$$_(Edit.down)(a)))(Fay$$_(Fay$$_(Edit.State)(a))(o));}if (Fay$$_($tmp1) instanceof Edit._SelUp) {return Fay$$_(Fay$$_(Edit.moveTo)(Fay$$_(Edit.up)(a)))(Fay$$_(Fay$$_(Edit.State)(a))(o));}if (Fay$$_($tmp1) instanceof Edit._SelLeft) {return Fay$$_(Fay$$_(Edit.moveTo)(Fay$$_(Edit.left)(a)))(Fay$$_(Fay$$_(Edit.State)(a))(o));}if (Fay$$_($tmp1) instanceof Edit._SelRight) {return Fay$$_(Fay$$_(Edit.moveTo)(Fay$$_(Edit.right)(a)))(Fay$$_(Fay$$_(Edit.State)(a))(o));}if (Fay$$_($tmp1) instanceof Edit._InsBefore) {var t = Fay$$_($tmp1).slot1;return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Edit.State)(a)))(Fay$$_(Fay$$_(Fay$$_(Edit.olinsertAt)(a))(t))(o));}if (Fay$$_($tmp1) instanceof Edit._InsAfter) {var t = Fay$$_($tmp1).slot1;return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Edit.State)(Fay$$_(Edit.down)(a))))(Fay$$_(Fay$$_(Fay$$_(Edit.olinsertAt)(Fay$$_(Edit.down)(a)))(t))(o));}if (Fay$$_($tmp1) instanceof Edit._InsAbove) {var t = Fay$$_($tmp1).slot1;return Fay$$_(Fay$$_(Fay$$_(Edit.insAbove)(t))(a))(o);}if (Fay$$_($tmp1) instanceof Edit._InsBelow) {var t = Fay$$_($tmp1).slot1;return Fay$$_(Fay$$_(Fay$$_(Edit.insBelow)(t))(a))(o);}return (function(){ throw (["unhandled case",$tmp1]); })();})(op);}throw ["unhandled case in apply",[$p1,$p2]];});};};Fay$$objConcat(Fay$$fayToJsHash,{"State": function(type,argTypes,_obj){var obj_ = {"instance": "State"};var obj_slot1 = Fay$$fayToJs(["user","Addr",[]],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}var obj_slot2 = Fay$$fayToJs(["user","OL",[]],_obj.slot2);if (undefined !== obj_slot2) {obj_['slot2'] = obj_slot2;}return obj_;},"SelDown": function(type,argTypes,_obj){var obj_ = {"instance": "SelDown"};return obj_;},"SelLeft": function(type,argTypes,_obj){var obj_ = {"instance": "SelLeft"};return obj_;},"SelUp": function(type,argTypes,_obj){var obj_ = {"instance": "SelUp"};return obj_;},"SelRight": function(type,argTypes,_obj){var obj_ = {"instance": "SelRight"};return obj_;},"Select": function(type,argTypes,_obj){var obj_ = {"instance": "Select"};var obj_slot1 = Fay$$fayToJs(["user","Addr",[]],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"Edit": function(type,argTypes,_obj){var obj_ = {"instance": "Edit"};var obj_slot1 = Fay$$fayToJs(["user","OLStr",[]],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"Delete": function(type,argTypes,_obj){var obj_ = {"instance": "Delete"};return obj_;},"Nada": function(type,argTypes,_obj){var obj_ = {"instance": "Nada"};return obj_;},"InsBefore": function(type,argTypes,_obj){var obj_ = {"instance": "InsBefore"};var obj_slot1 = Fay$$fayToJs(["user","OLStr",[]],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"InsAfter": function(type,argTypes,_obj){var obj_ = {"instance": "InsAfter"};var obj_slot1 = Fay$$fayToJs(["user","OLStr",[]],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"InsAbove": function(type,argTypes,_obj){var obj_ = {"instance": "InsAbove"};var obj_slot1 = Fay$$fayToJs(["user","OLStr",[]],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"InsBelow": function(type,argTypes,_obj){var obj_ = {"instance": "InsBelow"};var obj_slot1 = Fay$$fayToJs(["user","OLStr",[]],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}});Fay$$objConcat(Fay$$jsToFayHash,{"State": function(type,argTypes,obj){return new Edit._State(Fay$$jsToFay(["user","Addr",[]],obj["slot1"]),Fay$$jsToFay(["user","OL",[]],obj["slot2"]));},"SelDown": function(type,argTypes,obj){return new Edit._SelDown();},"SelLeft": function(type,argTypes,obj){return new Edit._SelLeft();},"SelUp": function(type,argTypes,obj){return new Edit._SelUp();},"SelRight": function(type,argTypes,obj){return new Edit._SelRight();},"Select": function(type,argTypes,obj){return new Edit._Select(Fay$$jsToFay(["user","Addr",[]],obj["slot1"]));},"Edit": function(type,argTypes,obj){return new Edit._Edit(Fay$$jsToFay(["user","OLStr",[]],obj["slot1"]));},"Delete": function(type,argTypes,obj){return new Edit._Delete();},"Nada": function(type,argTypes,obj){return new Edit._Nada();},"InsBefore": function(type,argTypes,obj){return new Edit._InsBefore(Fay$$jsToFay(["user","OLStr",[]],obj["slot1"]));},"InsAfter": function(type,argTypes,obj){return new Edit._InsAfter(Fay$$jsToFay(["user","OLStr",[]],obj["slot1"]));},"InsAbove": function(type,argTypes,obj){return new Edit._InsAbove(Fay$$jsToFay(["user","OLStr",[]],obj["slot1"]));},"InsBelow": function(type,argTypes,obj){return new Edit._InsBelow(Fay$$jsToFay(["user","OLStr",[]],obj["slot1"]));}});var UI = {};UI.setSel = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof OL._Addr) {var is = Fay$$_($p1).slot1;return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(JS.setVar)(Fay$$list("addr"))))(Fay$$_(Fay$$_(Prelude.$36$)(Prelude.concat))(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Util.myintersperse)(Fay$$list(","))))(Fay$$_(Fay$$_(Prelude.map)(Prelude.show))(is))));}throw ["unhandled case in setSel",[$p1]];});};UI.getSel = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(JS.getVar)(Fay$$list("addr"))))(Fay$$_(Fay$$_(Prelude.$46$)(Fay$$$_return))(OL.addrread));});UI.setOutline = function($p1){return new Fay$$$(function(){var o = $p1;return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(JS.setVar)(Fay$$list("ol"))))(Fay$$_(OL.olshow)(o));});};UI.getOutline = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(JS.getVar)(Fay$$list("ol"))))(Fay$$_(Fay$$_(Prelude.$46$)(Fay$$$_return))(OL.olread));});UI.getState = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$bind)(UI.getSel))(function($p1){var s = $p1;return Fay$$_(Fay$$_(Fay$$bind)(UI.getOutline))(function($p1){var o = $p1;return Fay$$_(Fay$$$_return)(Fay$$_(Fay$$_(Edit.State)(s))(o));});});});UI.setState = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof Edit._State) {var a = Fay$$_($p1).slot1;var o = Fay$$_($p1).slot2;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(UI.setSel)(a)))(Fay$$_(UI.setOutline)(o));}throw ["unhandled case in setState",[$p1]];});};UI.setAttrs = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {var n = $p1;return Fay$$_(Fay$$$_return)(Fay$$unit);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(Fay$$_($tmp1.car),2)) {var k = Fay$$index(0,Fay$$_($tmp1.car));var v = Fay$$index(1,Fay$$_($tmp1.car));var as = $tmp1.cdr;var n = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(JS.setAttr)(n))(k))(v)))(Fay$$_(Fay$$_(UI.setAttrs)(n))(as));}}throw ["unhandled case in setAttrs",[$p1,$p2]];});};};UI.getOutlineNode = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(JS.gid)(Fay$$list("outline"))))(function($p1){var ol = $p1;return (function($tmp1){if (Fay$$_($tmp1) instanceof Fay.FFI._Null) {return Fay$$_(Prelude.error)(Fay$$list("There has to be a node with id=“outline” in the document."));}if (Fay$$_($tmp1) instanceof Fay.FFI._Nullable) {var node = Fay$$_($tmp1).slot1;return Fay$$_(Fay$$$_return)(node);}return (function(){ throw (["unhandled case",$tmp1]); })();})(ol);});});UI.clearPage = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$bind)(UI.getOutlineNode))(function($p1){var o = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(JS.getChilds)(o)))(Fay$$_(Util.iter)(Fay$$_(JS.removeChild)(o)));});});UI.writePage = function($p1){return new Fay$$$(function(){var d = $p1;return Fay$$_(Fay$$_(Fay$$bind)(UI.getOutlineNode))(function($p1){var o = $p1;return Fay$$_(Fay$$_(JS.appendChild)(o))(d);});});};UI.setPage = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$then)(UI.clearPage))(Fay$$_(UI.writePage)(x));});};UI.gendom = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof DOM._Node) {var tag = Fay$$_($p1).slot1;var attrs = Fay$$_($p1).slot2;var txt = Fay$$_($p1).slot3;var childs = Fay$$_($p1).slot4;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(JS.mknode)(tag)))(function($p1){var n = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(UI.setAttrs)(n))(attrs)))(Fay$$_(Fay$$_(Fay$$then)((function($tmp1){if (Fay$$_($tmp1) instanceof Prelude._Nothing) {return Fay$$_(Fay$$$_return)(Fay$$unit);}if (Fay$$_($tmp1) instanceof Prelude._Just) {var s = Fay$$_($tmp1).slot1;return Fay$$_(Fay$$_(JS.setText)(n))(s);}return (function(){ throw (["unhandled case",$tmp1]); })();})(txt)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Util.mymapM)(UI.gendom))(childs)))(Fay$$_(Util.iter)(Fay$$_(JS.appendChild)(n)))))(Fay$$_(Fay$$$_return)(n))));});}throw ["unhandled case in gendom",[$p1]];});};UI.prompt$39$ = function($p1){return function($p2){return new Fay$$$(function(){var default$39$ = $p2;var q = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(JS.prompt)(q))(default$39$)))(function($p1){var x = $p1;return (function($tmp1){if (Fay$$_($tmp1) instanceof Fay.FFI._Null) {return Fay$$_(Fay$$$_return)(default$39$);}if (Fay$$_($tmp1) instanceof Fay.FFI._Nullable) {var t = Fay$$_($tmp1).slot1;return Fay$$_(Fay$$$_return)(t);}return (function(){ throw (["unhandled case",$tmp1]); })();})(x);});});};};UI.nodeAddr = function($p1){return new Fay$$$(function(){var n = $p1;return (function(){var r = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof Fay.FFI._Null) {return Fay$$_(Prelude.error)(Fay$$list("Internal Error: Clickable node has invalid ID"));}if (Fay$$_($p1) instanceof Fay.FFI._Nullable) {var i = Fay$$_($p1).slot1;return Fay$$_(Fay$$$_return)(Fay$$_(DOM.idAddr)(i));}throw ["unhandled case in r",[$p1]];});};return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(JS.getId)(n)))(r);})();});};UI.select = function($p1){return new Fay$$$(function(){var n = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(UI.nodeAddr)(n)))(UI.setSel)))(UI.buildit);});};UI.editKey = function($p1){return function($p2){return new Fay$$$(function(){var k = $p2;var n = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(JS.getText)(n)))(function($p1){var t = $p1;return (function($tmp1){if (Fay$$equal($tmp1,Fay$$list("h"))) {return Fay$$_(Fay$$$_return)(Edit.SelLeft);}if (Fay$$equal($tmp1,Fay$$list("l"))) {return Fay$$_(Fay$$$_return)(Edit.SelRight);}if (Fay$$equal($tmp1,Fay$$list("k"))) {return Fay$$_(Fay$$$_return)(Edit.SelUp);}if (Fay$$equal($tmp1,Fay$$list("j"))) {return Fay$$_(Fay$$$_return)(Edit.SelDown);}if (Fay$$equal($tmp1,Fay$$list("d"))) {return Fay$$_(Fay$$$_return)(Edit.Delete);}if (Fay$$equal($tmp1,Fay$$list("i"))) {return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$$_return))(Fay$$_(Fay$$_(Prelude.$36$)(Edit.InsBefore))(Fay$$_(OL.ols)(Fay$$list(""))));}if (Fay$$equal($tmp1,Fay$$list("a"))) {return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$$_return))(Fay$$_(Fay$$_(Prelude.$36$)(Edit.InsAfter))(Fay$$_(OL.ols)(Fay$$list(""))));}if (Fay$$equal($tmp1,Fay$$list("o"))) {return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$$_return))(Fay$$_(Fay$$_(Prelude.$36$)(Edit.InsBelow))(Fay$$_(OL.ols)(Fay$$list(""))));}if (Fay$$equal($tmp1,Fay$$list("O"))) {return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$$_return))(Fay$$_(Fay$$_(Prelude.$36$)(Edit.InsAbove))(Fay$$_(OL.ols)(Fay$$list(""))));}if (Fay$$equal($tmp1,Fay$$list("r"))) {return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(UI.prompt$39$)(Fay$$list("Replace Text")))(t)))(Fay$$_(Fay$$_(Prelude.$46$)(Fay$$$_return))(Fay$$_(Fay$$_(Prelude.$46$)(Edit.Edit))(OL.ols)));}if (Fay$$equal($tmp1,Fay$$list("\r"))) {return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(UI.prompt$39$)(Fay$$list("Replace Text")))(t)))(Fay$$_(Fay$$_(Prelude.$46$)(Fay$$$_return))(Fay$$_(Fay$$_(Prelude.$46$)(Edit.Edit))(OL.ols)));}return Fay$$_(Fay$$$_return)(Edit.Nada);})(k);});});};};UI.setupKeys = new Fay$$$(function(){return new Fay$$$(function(){var dumpText = function($p1){return new Fay$$$(function(){var t = $p1;return (function(){var payload = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$_(Fay$$_(DOM.Node)(Fay$$list("pre")))(null))(Fay$$_(Fay$$_(Prelude.$36$)(Prelude.Just))(Fay$$_(Fay$$_(Prelude.$43$$43$)(t))(Fay$$list("\n")))))(null);});return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(UI.gendom)(payload)))(UI.writePage);})();});};var r = function($p1){return new Fay$$$(function(){if (Fay$$equal($p1,Fay$$list("!"))) {return Fay$$_(Fay$$_(Fay$$bind)(UI.getState))(function($p1){if (Fay$$_($p1) instanceof Edit._State) {var a = Fay$$_($p1).slot1;var ol = Fay$$_($p1).slot2;return Fay$$_(Fay$$_(Prelude.$36$)(dumpText))(Fay$$_(Fay$$_(Prelude.$36$)(OL.olshow))(ol));}throw ["unhandled case",$p1];});}var k = $p1;return Fay$$_(Fay$$_(Fay$$bind)(UI.getSel))(function($p1){var a = $p1;return Fay$$_(Fay$$_(Fay$$bind)(UI.getState))(function($p1){var s = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Prelude.$36$)(JS.gid))(Fay$$_(DOM.addrId)(a))))(function($p1){var n$39$ = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$$_return))((function($tmp1){if (Fay$$_($tmp1) instanceof Fay.FFI._Null) {return Fay$$_(Prelude.error)(Fay$$list("bad addr"));}if (Fay$$_($tmp1) instanceof Fay.FFI._Nullable) {var z = Fay$$_($tmp1).slot1;return z;}return (function(){ throw (["unhandled case",$tmp1]); })();})(n$39$))))(function($p1){var n = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(UI.editKey)(n))(k)))(function($p1){var op = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude.$36$)(UI.setState))(Fay$$_(Fay$$_(Edit.apply)(op))(s))))(UI.buildit);});});});});});});};return Fay$$_(JS.onKeyPress)(r);});});UI.fixAddr = function($p1){return function($p2){return new Fay$$$(function(){var addr = $p2;var ol = $p1;return Fay$$_(Fay$$_(Fay$$_(Edit.validSel)(addr))(ol)) ? Fay$$_(Fay$$$_return)(addr) : Fay$$_(Fay$$_(Fay$$then)(Fay$$_(UI.setSel)(Fay$$_(OL.Addr)(null))))(Fay$$_(Fay$$$_return)(Fay$$_(OL.Addr)(null)));});};};UI.buildit = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$bind)(UI.getOutline))(function($p1){var ol = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Fay$$bind)(UI.getSel))(Fay$$_(UI.fixAddr)(ol))))(function($p1){var a = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(UI.gendom)(Fay$$_(Fay$$_(DOM.olDom)(a))(ol))))(UI.setPage)))(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(JS.byClass)(Fay$$list("unselected"))))(Fay$$_(Util.iter)(function($p1){var e = $p1;return Fay$$_(Fay$$_(JS.onClick)(e))(Fay$$_(UI.select)(e));})));});});});UI.main = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$then)(JS.initVars))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude.$36$)(UI.setSel))(Fay$$_(OL.Addr)(null))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(UI.setOutline)(OL.olexample)))(Fay$$_(Fay$$_(Fay$$then)(UI.buildit))(UI.setupKeys))));});
;
Fay$$_(UI.main);

