/*
 * Copyright 2002-2013 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.client;

import java.net.InetSocketAddress;
import java.util.List;
import java.util.Properties;

import com.gemstone.gemfire.cache.client.ClientCacheFactory;

/**
 * @author David Turanski
 *
 */
public class GemfireServerDataSource extends AbstractGemfireDataSource {
	
	public GemfireServerDataSource(ClientCacheFactory clientCacheFactory) {
		super(clientCacheFactory);
	}
	
	public GemfireServerDataSource(String host, int port, Properties properties) {
		super(host, port, properties);
	}
	
	public GemfireServerDataSource(List<InetSocketAddress> remoteConnections, Properties properties) {
		 super(remoteConnections, properties);
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.client.AbstractGemfireDataSource#addRemoteConnection(com.gemstone.gemfire.cache.client.ClientCacheFactory, java.lang.String, int)
	 */
	@Override
	protected void addRemoteConnection(ClientCacheFactory clientCacheFactory, String host, int port) {
		clientCacheFactory.addPoolServer(host, port);
	}
}