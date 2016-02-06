using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Newegg.Rebate
{
    public class SimpleRebateByNum : AbstractRebateByNum
    {
        public SimpleRebateByNum(decimal amount)
        {
            this.Amount = amount;
        }

        public override decimal GetRebateAmt(RebateBaseInfo rebateBaseInfo)
        {
            return rebateBaseInfo.GoodsNumber * Amount;
        }
    }
}
