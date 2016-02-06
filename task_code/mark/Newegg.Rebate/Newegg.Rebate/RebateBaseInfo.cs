using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Newegg.Rebate
{
    public class RebateBaseInfo
    {
        public int GoodsNumber { get; set; }

        public decimal Amount { get; set; }

        public RebateType Type { get; set; }

        public RebateByAmtStrategy ByAmtStrategyType { get; set; }

        public RebateByNumStrategy ByNumStrategyType { get; set; }

        public decimal Percent { get; set; }

        public decimal RebateAmount { get; set; }
    }
}
