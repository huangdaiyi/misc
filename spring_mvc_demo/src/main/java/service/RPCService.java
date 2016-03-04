package service;

import javax.servlet.http.HttpServletRequest;

import exception.MetadataException;

public interface RPCService {

	public Object call(String serverName, HttpServletRequest request, Object body) throws MetadataException;

}
