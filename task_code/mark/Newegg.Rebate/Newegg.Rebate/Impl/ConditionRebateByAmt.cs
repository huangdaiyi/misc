using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Newegg.Rebate
{
    public class ConditionRebateByAmt : AbstractRebateByAmt
    {
        public ConditionRebateByAmt(decimal percent)
        {
            this.Percent = percent;
        }

        public override decimal GetRebateAmt(RebateBaseInfo rebateBaseInfo)
        {
            return rebateBaseInfo.Amount > 200 ? rebateBaseInfo.Amount * Percent : 0;
        }
    }
}
