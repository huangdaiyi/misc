using Newegg.Rebate;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace App
{
    class Program
    {
        static void Main(string[] args)
        {
            //这里mock数据
            RebateBaseInfo baseInfo = new RebateBaseInfo() 
            {
                GoodsNumber = 1,
                RebateAmount = 5.86m,
                Type = RebateType.ByNumber,
                ByNumStrategyType = RebateByNumStrategy.Simple
            };
            IRebate rebator = RebateFactory.GetRebator(baseInfo);
            Console.Write(rebator.GetRebateAmt(baseInfo));
            Console.Read();
        }
    }
}
