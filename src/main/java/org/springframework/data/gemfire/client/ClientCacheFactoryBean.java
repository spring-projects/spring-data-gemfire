/*
 * Copyright 2011-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.client;

import java.net.InetSocketAddress;
import java.util.List;
import java.util.Properties;

import org.springframework.beans.factory.BeanInitializationException;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.config.GemfireConstants;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientCacheFactory;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.client.PoolManager;
import com.gemstone.gemfire.pdx.PdxSerializer;

/**
 * FactoryBean dedicated to creating client caches (caches for client JVMs).
 * Acts an utility class (as client caches are a subset with a particular
 * configuration of the generic cache).
 * 
 * @author Costin Leau
 * @author Lyndon Adams
 * @author John Blum
 */
public class ClientCacheFactoryBean extends CacheFactoryBean {

	/**
	 * Inner class to avoid a hard dependency on the GemFire 6.6 API.
	 * 
	 * @author Costin Leau
	 */
	private class PdxOptions implements Runnable {

		private ClientCacheFactory factory;

		PdxOptions(ClientCacheFactory factory) {
			this.factory = factory;
		}

		public void run() {
			if (pdxSerializer != null) {
				Assert.isAssignable(PdxSerializer.class,
						pdxSerializer.getClass(), "Invalid pdx serializer used");
				factory.setPdxSerializer((PdxSerializer) pdxSerializer);
			}
			if (pdxDiskStoreName != null) {
				factory.setPdxDiskStore(pdxDiskStoreName);
			}
			if (pdxIgnoreUnreadFields != null) {
				factory.setPdxIgnoreUnreadFields(pdxIgnoreUnreadFields);
			}
			if (pdxPersistent != null) {
				factory.setPdxPersistent(pdxPersistent);
			}
			if (pdxReadSerialized != null) {
				factory.setPdxReadSerialized(pdxReadSerialized);
			}
			
		}
	}

	private String poolName;
	private Pool pool;
	
	protected Boolean readyForEvents = false;

	@Override
	protected GemFireCache createCache(Object factory) {
		ClientCacheFactory clientCacheFactory = (ClientCacheFactory) factory;

		initializePool(clientCacheFactory);
		
		// Now create the cache
		GemFireCache cache = clientCacheFactory.create();
		
		// Register for events after pool/regions been created and iff non-durable client
		readyForEvents();
		
		// Return the cache
		return cache;
	}

	@Override
	protected Object createFactory(Properties gemfireProperties) {
		return new ClientCacheFactory(gemfireProperties);
	}

	@Override
	protected GemFireCache fetchCache() {
		return ClientCacheFactory.getAnyInstance();
	}
	
	private void initializePool(ClientCacheFactory ccf) {
		Pool p = pool;

		if (p == null) {
			if (StringUtils.hasText(poolName)) {
				p = PoolManager.find(poolName);
			}
			
		   // Bind this client cache to a pool that hasn't been created yet.
			if (p == null) {
				PoolFactoryBean.connectToTemporaryDs(this.properties);
			}
			
			if (StringUtils.hasText(poolName)) {
				try {

					getBeanFactory().isTypeMatch(poolName, Pool.class);
				} catch (Exception e) {
					String msg = "No bean found  with name " + poolName
							+ " of type " + Pool.class.getName();
					if (poolName
							.equals(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME)) {
						msg += ". A client cache requires a pool";
					}
					throw new BeanInitializationException(msg);
				}
				p = getBeanFactory().getBean(poolName, Pool.class);
			} else {
				if (log.isDebugEnabled()) {
					log.debug("Checking for a unique pool");
				}
				p = getBeanFactory().getBean(Pool.class);
				this.poolName = p.getName();
			}
		 
		}

		if (p != null) {
			// copy the pool settings - this way if the pool is not found, at
			// least the cache will have a similar config
			ccf.setPoolFreeConnectionTimeout(p.getFreeConnectionTimeout());
			ccf.setPoolIdleTimeout(p.getIdleTimeout());
			ccf.setPoolLoadConditioningInterval(p.getLoadConditioningInterval());
			ccf.setPoolMaxConnections(p.getMaxConnections());
			ccf.setPoolMinConnections(p.getMinConnections());
			ccf.setPoolMultiuserAuthentication(p.getMultiuserAuthentication());
			ccf.setPoolPingInterval(p.getPingInterval());
			ccf.setPoolPRSingleHopEnabled(p.getPRSingleHopEnabled());
			ccf.setPoolReadTimeout(p.getReadTimeout());
			ccf.setPoolRetryAttempts(p.getRetryAttempts());
			ccf.setPoolServerGroup(p.getServerGroup());
			ccf.setPoolSocketBufferSize(p.getSocketBufferSize());
			ccf.setPoolStatisticInterval(p.getStatisticInterval());
			ccf.setPoolSubscriptionAckInterval(p.getSubscriptionAckInterval());
			ccf.setPoolSubscriptionEnabled(p.getSubscriptionEnabled());
			ccf.setPoolSubscriptionMessageTrackingTimeout(p
					.getSubscriptionMessageTrackingTimeout());
			ccf.setPoolSubscriptionRedundancy(p.getSubscriptionRedundancy());
			ccf.setPoolThreadLocalConnections(p.getThreadLocalConnections());

			List<InetSocketAddress> locators = p.getLocators();
			if (locators != null) {
				for (InetSocketAddress inet : locators) {
					ccf.addPoolLocator(inet.getHostName(), inet.getPort());
				}
			}

			List<InetSocketAddress> servers = p.getServers();
			if (servers != null) {
				for (InetSocketAddress inet : servers) {
					ccf.addPoolServer(inet.getHostName(), inet.getPort());
				}
			}
		}
	}

	@Override
	protected void postProcessPropertiesBeforeInitialization(final Properties gemfireProperties) {
	}

	@Override
	public final Boolean getEnableAutoReconnect() {
		return Boolean.FALSE;
	}

	@Override
	public final void setEnableAutoReconnect(final Boolean enableAutoReconnect) {
		throw new UnsupportedOperationException("Auto-reconnect is not supported on ClientCache.");
	}

	/**
	 * Sets the pool used by this client.
	 *
	 * @param pool the GemFire pool used by the Client Cache to obtain connections to the GemFire cluster.
	 */
	public void setPool(Pool pool) {
		Assert.notNull(pool, "pool cannot be null");
		this.pool = pool;
	}

	/**
	 * Sets the pool name used by this client.
	 *
	 * @param poolName set the name of the GemFire Pool used by the GemFire Client Cache.
	 */
	public void setPoolName(String poolName) {
		Assert.hasText(poolName, "pool name is required");
		this.poolName = poolName;
	}

	/**
	 * Set the readyForEvents flag.
	 *
	 * @param readyForEvents sets a boolean flag to notify the server that this durable client is ready
	 * to receive updates.
	 * @see #getReadyForEvents()
	 */
	public void setReadyForEvents(Boolean readyForEvents){
		this.readyForEvents = readyForEvents;
	}

	public Boolean getReadyForEvents(){
		return this.readyForEvents;
	}

	@Override
	protected void applyPdxOptions(Object factory) {
		if (factory instanceof ClientCacheFactory) {
			new PdxOptions((ClientCacheFactory) factory).run();
		}
	}

	/**
	 * Inform the GemFire cluster that this client cache is ready to receive events.
	 */
	private void readyForEvents(){
		ClientCache clientCache = ClientCacheFactory.getAnyInstance();

		if (Boolean.TRUE.equals(readyForEvents) && !clientCache.isClosed()) {
			try {
				clientCache.readyForEvents();
			}
			catch (IllegalStateException ignore) {
				// cannot be called for a non-durable client so exception is thrown
			}
		}
	}

}
