using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Newegg.Rebate
{
    public class RebateByNumContext
    {
        public IRebate GetRebator(RebateBaseInfo baseInfo)
        {
            switch (baseInfo.ByNumStrategyType)
            {
                case RebateByNumStrategy.Simple:
                    return new SimpleRebateByNum(baseInfo.RebateAmount);
                case RebateByNumStrategy.Condition:
                    return new ConditionRebateByNum(baseInfo.RebateAmount);
                default:
                    return null;
            }
        }
    }
}
