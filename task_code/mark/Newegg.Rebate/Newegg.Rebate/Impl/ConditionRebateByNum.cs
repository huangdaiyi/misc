using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Newegg.Rebate
{
    public class ConditionRebateByNum : AbstractRebateByNum
    {
        public ConditionRebateByNum(decimal amount)
        {
            this.Amount = amount;
        }

        public override decimal GetRebateAmt(RebateBaseInfo rebateBaseInfo)
        {
            return rebateBaseInfo.GoodsNumber > 100 ? rebateBaseInfo.GoodsNumber * Amount : 0;
        }
    }
}
