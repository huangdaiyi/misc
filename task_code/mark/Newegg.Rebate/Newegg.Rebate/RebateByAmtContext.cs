using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Newegg.Rebate
{
    public class RebateByAmtContext
    {
        public IRebate GetRebator(RebateBaseInfo baseInfo)
        {
            switch (baseInfo.ByAmtStrategyType)
            {
                case RebateByAmtStrategy.Simple:
                    return new SimpleRebateByAmt(baseInfo.Percent);
                case RebateByAmtStrategy.Condition:
                    return new ConditionRebateByAmt(baseInfo.Percent);
                default :
                    return null;
            }
        }
    }
}
