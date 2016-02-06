using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Newegg.Rebate
{
    public static class RebateFactory
    {
        public static IRebate GetRebator(RebateBaseInfo baseInfo)
        {
            switch (baseInfo.Type)
            {
                case RebateType.ByAmount:
                    var rebateByAmtContext = new RebateByAmtContext();
                    return rebateByAmtContext.GetRebator(baseInfo);
                case RebateType.ByNumber:
                    var rebateByNumContext = new RebateByNumContext();
                    return rebateByNumContext.GetRebator(baseInfo);
                default:
                    return null;
            }
        }
    }
}
