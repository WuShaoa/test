 const curry = (func) => {
  return curriedFn = (...args) => {
    // 判断实参、形参个数
    if (args.length < func.length) {
      return (...args1) => {
        // 这个func用来获取arguments
        //下面三种写法都可以
        // return curriedFn(...args.concat(Array.from(args1)));
        // return curriedFn(...args.concat(...args1));
        return curriedFn(...args.concat(args1));
      }
    }
    return func(...args);
  }
 }

 const getSum = (a, b, c) => {
  return a + b + c;
 };
 const curried = curry(getSum);

 console.log(curried(1, 2, 3));
 console.log(curried(1,2)(3));
 console.log(curried(1)(2, 3));
 console.log(curried(1)(2)(3));


//from：https://juejin.cn/post/7040813656371625998
