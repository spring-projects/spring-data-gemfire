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
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.data.gemfire.function.execution.GemfireFunctionOperations;
import org.springframework.data.gemfire.function.execution.GemfireOnServersFunctionTemplate;
import org.springframework.data.gemfire.repository.support.ListRegionsOnServerFunction;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientCacheFactory;
import com.gemstone.gemfire.cache.client.ClientRegionFactory;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.query.QueryService;
import com.gemstone.gemfire.distributed.DistributedSystem;

/**
 * @author David Turanski
 *
 */
public abstract class AbstractGemfireDataSource {
	protected Log logger = LogFactory.getLog(getClass());
	protected ClientCache cache;

	public AbstractGemfireDataSource(ClientCacheFactory clientCacheFactory) {
		this.cache = clientCacheFactory.create();
	}
	
	public AbstractGemfireDataSource(String host, int port, Properties properties) {
		ClientCacheFactory clientCacheFactory = new ClientCacheFactory();
		addRemoteConnection(clientCacheFactory, host, port);
		initializeClientCache(clientCacheFactory,properties);
		this.cache = clientCacheFactory.create();
		createClientRegions();
	}

	public AbstractGemfireDataSource(List<InetSocketAddress> remoteConnections, Properties properties) {
		
		ClientCacheFactory clientCacheFactory = new ClientCacheFactory();
		for (InetSocketAddress remoteConnection: remoteConnections) {
			addRemoteConnection(clientCacheFactory, remoteConnection.getHostName(), remoteConnection.getPort());
		}
		initializeClientCache(clientCacheFactory,properties);
		this.cache = clientCacheFactory.create();
		createClientRegions();
	}
	
	protected abstract void addRemoteConnection(ClientCacheFactory clientCacheFactory, String host, int port);
	 
	public void connect() {
		this.connect();
	}

	public String getName() {
		return cache.getName();
	}

	public String getServerGroup() {
		return cache.getDefaultPool().getServerGroup();
	}

	 
	public List<InetSocketAddress> getLocators() {
		return cache.getDefaultPool().getLocators();
	}

	 
	public List<InetSocketAddress> getServers() {
		return cache.getDefaultPool().getServers();
	}

  
 
	public QueryService getQueryService() {
		return cache.getQueryService();
	}

	
	public QueryService getQueryService(String poolName) {
		return cache.getQueryService(poolName);
	}

	 
	public QueryService getLocalQueryService() {
		return cache.getLocalQueryService();
	}
 
 
	public void close(boolean keepalive) {
		cache.close(keepalive);
	}

	public void readyForEvents() {
		cache.readyForEvents();
		
	}

	public Set<InetSocketAddress> getCurrentServers() {
		return cache.getCurrentServers();
	}

 
	public Pool getDefaultPool() {
		return cache.getDefaultPool();
	}
 
	public DistributedSystem getDistributedSystem() {
		return cache.getDistributedSystem();
	}



	public <K, V> Region<K, V> getRegion(String path) {
		return cache.getRegion(path);
	}

	 
	public Set<Region<?, ?>> rootRegions() {
		return cache.rootRegions();
	}

 
	public void close() {
		cache.close();
	}

	 
	public boolean isClosed() {
		return cache.isClosed();
	}
	
	/**
	 * 
	 */
	private void createClientRegions() {
		GemfireFunctionOperations template = new GemfireOnServersFunctionTemplate(cache);
		Iterable<String> regionNames = template.executeAndExtract(new ListRegionsOnServerFunction());
		
		ClientRegionFactory<?,?> clientRegionFactory = null;
		if (regionNames !=null && regionNames.iterator().hasNext()) {
			clientRegionFactory = cache.createClientRegionFactory(ClientRegionShortcut.PROXY);
		}
		
		for (String regionName: regionNames) {
			if (logger.isDebugEnabled()) {
				logger.debug("creating client region for " + regionName);
				clientRegionFactory.create(regionName);
			}
		}
		
	}
	
	/**
	 * @param clientCacheFactory
	 * @param properties
	 */
	private void initializeClientCache(ClientCacheFactory clientCacheFactory, Properties properties) {
		// TODO Auto-generated method stub
		
	}

}
