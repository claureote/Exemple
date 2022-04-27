plug Basket is { float[]  W | float[] signal S | int N};
plug PerfEquity#Plg is {float signal S| date event t0};
plug SingleFloatSignal#Plg is {float signal value};
type strike is float;
type refEquity is string;
type diBarrier is float;


block constantFloatSignal#Blk returns SingleFloatSignal#Plg
{
     float value;
      constantFloatSignal#Blk.value = always value;
}

block basketEquity returns Basket
{
    int Size;
    refEquity[Size] refs;
    float[Size] W;

    basketEquity.N = Size;
    basketEquity.W = W;
    basketEquity.S =  equity refs;
}

block perf of Basket B returns PerfEquity#Plg  {
     date t0;
     datelist cal;
     perf.t0 = the t0;
     //t = calendar cal;

    //S_(t) = sum_(i=1 .. B.N)  ( B.W[i] * B.S[i]_(t) / B.S_(t0)[i] ) - 100% ;
    perf.S_(INIT) = INACTIVE ;
     perf.S_(t >= t0 ) = sum_(i=1 .. B.N)  ( B.W[i] * B.S[i]_(t) / B.S[i]_(t0) ) - 100% ;
}

 block worstperf of Basket B returns PerfEquity#Plg  {
    date striking;
    t0 = the striking;
    S = B.S;
    worstperf.t0 = t0;
    worstperf.S_(INIT) = INACTIVE ;
    worstperf.S_(t>= t0 )=  Min_( i=1..B.N ) (  S[i]_(t) / S[i]_(t0)  - 100% );
}

block rainbowperf of Basket B returns PerfEquity#Plg  {
    date t0;
    perfIndiv_(INIT) = INACTIVE ;
    perfIndiv[i]_(t >= the t0 ) =   B.S[i]_(t) / B.S[i]_(t0)  - 100%  ;
   rainbowperf.S_(INIT) = INACTIVE ;
   rainbowperf.S_(t>= t0 )= sum_(i=1 .. B.N)  (perfIndiv[i]_(t) ) ;
}


Template Coupon#Tpl uses a block called coupon returning SingleFloatSignal#Plg
{
    datelist pay, fixing;
    string payCur;
    paydates =calendar pay;
    Coupon#Tpl pays coupon.value_(fixing) on paydates in currency payCur;

}



Template Autocall uses a block  called Perf returning PerfEquity#Plg
                     and delivers a Coupon#Tpl called coupon , a Generic called Final,
                    a Coupon#Tpl called OnCallRedemption
{
    strike K;
    date begining,end;
    datelist call,pay;
    string payCur, refCur;


    cur =  currency refCur ;

    startDate = the begining;
    callDates  = calendar call;
    payDate = calendar pay;

    //float coupon_(call);

    callEvent =  next payDate following ( (when ((value of Perf.S on callDates) > K)));
    redemptionCoupon supersedes OnCallRedemption (pay:=pay,payCur:=refCur,fixing:=call);
    redemptionCoupon terminates on callEvent;

    Autocall supersedes coupon (pay:=pay , payCur:=payCur,fixing:=call);
    Autocall gives Final on the end;

   Autocall terminates on callEvent and gives redemptionCoupon ;

}

Template call uses a block called Perf returning PerfEquity#Plg
{
    date obsDate,payDate;
    strike K;
    string cur;

    T = the obsDate;
    pay = the payDate;

    call pays max ( (Perf.S_(T) - K ) , 0 ) on pay in currency cur;
}

Template put uses a block called Perf returning PerfEquity#Plg
{
    date startDate,payDate;
    strike K;
    string cur;

    T = the startDate;
    pay = the payDate;

    put pays max ( (K - Perf.S_(T)) , 0 ) on pay in currency cur ;
}

Template downIn uses a block called Perf returning PerfEquity#Plg  and delivers a Generic called payout
{

    date startDate;
    datelist obsDate;
    diBarrier B;

    e_obsDates = calendar obsDate;

    pay = once (when (value of Perf.S on e_obsDates) < B) ;

    downIn gives payout on pay ;
}

FixCoupon#Pay is a Coupon#Tpl where used block coupon is a constantFloatSignal#Blk;

phoenix is a Autocall where
        additional parameter
             Autocallcur is a string

        used block
             Perf is a (worstperf of a (basketEquity tagged bask) tagged Worst)

        delivered
             coupon is a (FixCoupon#Pay whose Cur is AutocallCur) ,
             Final is a (downIn where
                            used block
                                Perf is Worst

                            delivered
                                payout  is a (put whose Cur is AutocallCur where
                                                used block
                                                    Perf is a basketperf of bask)),
             redemptionCoupon is a (FixCoupon#Pay whose Cur is AutocallCur) ;



