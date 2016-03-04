package filter.impl;

import java.io.IOException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class CorsFilter implements Filter {

	private String accessControlAllowOrigin = "*";
	private String accessControlAllowMethods = "POST, DELETE, GET, PUT, OPTIONS";
	private String accessControlAllowHeaders = "Origin, X-Requested-With, Content-Type, Accept, neweggbox-sso-token";

	public CorsFilter() {
		super();
	}

	@Override
	public void init(FilterConfig filterConfig) throws ServletException {
		String accessControlAllowOrigin = filterConfig.getInitParameter("Access-Control-Allow-Origin");
		String accessControlAllowMethods = filterConfig.getInitParameter("Access-Control-Allow-Methods");
		String accessControlAllowHeaders = filterConfig.getInitParameter("Access-Control-Allow-Headers");
		if (accessControlAllowOrigin != null && accessControlAllowOrigin.length() > 0) {
			this.accessControlAllowOrigin = accessControlAllowOrigin;
		}
		if (accessControlAllowMethods != null && accessControlAllowMethods.length() > 0) {
			this.accessControlAllowMethods = accessControlAllowMethods;
		}
		if (accessControlAllowHeaders != null && accessControlAllowHeaders.length() > 0) {
			this.accessControlAllowHeaders = accessControlAllowHeaders;
		}
	}

	@Override
	public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain) throws IOException, ServletException {
		HttpServletResponse response = (HttpServletResponse) res;

		// cors header
		response.setHeader("Access-Control-Allow-Origin", accessControlAllowOrigin);
		response.setHeader("Access-Control-Allow-Methods", accessControlAllowMethods);
		response.setHeader("Access-Control-Allow-Headers", accessControlAllowHeaders);

		// response options method check
		if ("OPTIONS".equalsIgnoreCase(((HttpServletRequest) req).getMethod())) {
			return;
		}
		chain.doFilter(req, res);
	}

	@Override
	public void destroy() {
	}

}
