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

import java.util.Set;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.query.QueryService;
import com.gemstone.gemfire.distributed.DistributedSystem;

/**
 * @author David Turanski
 *
 */
public interface GemfireDataSource  {
	
	public void connect();

	public String getName();

	public String getServerGroup();
	
	public QueryService getQueryService();
	
	public QueryService getQueryService(String poolName);
	 
	public QueryService getLocalQueryService();
 
	public void readyForEvents();

	public Pool getDefaultPool();
	
	public DistributedSystem getDistributedSystem();
 
	public <K, V> Region<K, V> getRegion(String path);

	public Set<Region<?, ?>> rootRegions();
 
	public void close();
	
	public void close(boolean keepalive);
	
	public boolean isClosed();
}
