/*
 * Copyright 2016-2019 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config.annotation.support;

import static com.gemstone.gemfire.internal.lang.ObjectUtils.defaultIfNull;
import static org.springframework.data.gemfire.util.SpringUtils.defaultIfEmpty;

import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.RegionShortcut;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.GenericRegionFactoryBean;
import org.springframework.data.gemfire.RegionLookupFactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.util.Assert;

/**
 * The {@link GemFireCacheTypeAwareRegionFactoryBean} class is a smart Spring {@link FactoryBean} that knows how to
 * create a client or server {@link Region} depending on whether the {@link GemFireCache} is
 * a {@link com.gemstone.gemfire.cache.client.ClientCache} or a peer {@link com.gemstone.gemfire.cache.Cache}.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.BeanFactoryAware
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.data.gemfire.RegionLookupFactoryBean
 * @see com.gemstone.gemfire.cache.GemFireCache
 * @see com.gemstone.gemfire.cache.Region
 * @since 1.9.0
 */
@SuppressWarnings("unused")
public class GemFireCacheTypeAwareRegionFactoryBean<K, V> extends RegionLookupFactoryBean<K, V>
		implements BeanFactoryAware {

	private GemFireCache gemfireCache;

	private BeanFactory beanFactory;

	private Boolean close = false;

	private Class<K> keyConstraint;
	private Class<V> valueConstraint;

	private ClientRegionShortcut clientRegionShortcut = ClientRegionShortcut.PROXY;

	private DataPolicy dataPolicy = DataPolicy.DEFAULT;

	private RegionAttributes<K, V> regionAttributes;

	private RegionShortcut serverRegionShortcut;

	private String poolName;
	private String regionName;

	/**
	 * @inheritDoc
	 */
	@Override
	public Region<K, V> lookupRegion(GemFireCache gemfireCache, String regionName) throws Exception {
		return (GemfireUtils.isClient(gemfireCache) ? newClientRegion(gemfireCache, regionName)
			: newServerRegion(gemfireCache, regionName));
	}

	/**
	 * Constructs a new client {@link Region} using the {@link ClientRegionFactoryBean}.
	 *
	 * @param gemfireCache reference to the {@link GemFireCache} used to create/initialize the factory
	 * used to create the client {@link Region}.
	 * @param regionName name given to the client {@link Region}.
	 * @return a new instance of a client {@link Region} with the given {@code regionName}.
	 * @throws Exception if the client {@link Region} could not be created.
	 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
	 * @see com.gemstone.gemfire.cache.GemFireCache
	 * @see com.gemstone.gemfire.cache.Region
	 */
	protected Region<K, V> newClientRegion(GemFireCache gemfireCache, String regionName) throws Exception {
		ClientRegionFactoryBean<K, V> clientRegion = new ClientRegionFactoryBean<K, V>();

		clientRegion.setAttributes(getRegionAttributes());
		clientRegion.setBeanFactory(getBeanFactory());
		clientRegion.setCache(gemfireCache);
		clientRegion.setClose(isClose());
		clientRegion.setKeyConstraint(getKeyConstraint());
		clientRegion.setPoolName(getPoolName());
		clientRegion.setRegionName(regionName);
		clientRegion.setShortcut(getClientRegionShortcut());
		clientRegion.setValueConstraint(getValueConstraint());
		clientRegion.afterPropertiesSet();

		return clientRegion.getObject();
	}

	/**
	 * Constructs a new server {@link Region} using the {@link GenericRegionFactoryBean}.
	 *
	 * @param gemfireCache reference to the {@link GemFireCache} used to create/initialize the factory
	 * used to create the server {@link Region}.
	 * @param regionName name given to the server {@link Region}.
	 * @return a new instance of a server {@link Region} with the given {@code regionName}.
	 * @throws Exception if the server {@link Region} could not be created.
	 * @see org.springframework.data.gemfire.GenericRegionFactoryBean
	 * @see com.gemstone.gemfire.cache.GemFireCache
	 * @see com.gemstone.gemfire.cache.Region
	 */
	protected Region<K, V> newServerRegion(GemFireCache gemfireCache, String regionName) throws Exception {
		GenericRegionFactoryBean<K, V> serverRegion = new GenericRegionFactoryBean<K, V>();

		serverRegion.setAttributes(getRegionAttributes());
		serverRegion.setCache(gemfireCache);
		serverRegion.setClose(isClose());
		serverRegion.setDataPolicy(getDataPolicy());
		serverRegion.setKeyConstraint(getKeyConstraint());
		serverRegion.setRegionName(regionName);
		serverRegion.setShortcut(getServerRegionShortcut());
		serverRegion.setValueConstraint(getValueConstraint());
		serverRegion.afterPropertiesSet();

		return serverRegion.getObject();
	}

	public void setAttributes(RegionAttributes<K, V> regionAttributes) {
		this.regionAttributes = regionAttributes;
	}

	protected RegionAttributes<K, V> getRegionAttributes() {
		return this.regionAttributes;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}

	protected BeanFactory getBeanFactory() {
		Assert.state(this.beanFactory != null, "BeanFactory was not properly initialized");
		return this.beanFactory;
	}

	public void setClientRegionShortcut(ClientRegionShortcut clientRegionShortcut) {
		this.clientRegionShortcut = clientRegionShortcut;
	}

	protected ClientRegionShortcut getClientRegionShortcut() {
		return defaultIfNull(this.clientRegionShortcut, ClientRegionShortcut.PROXY);
	}

	public void setClose(Boolean close) {
		this.close = close;
	}

	protected Boolean getClose() {
		return this.close;
	}

	protected boolean isClose() {
		return Boolean.TRUE.equals(getClose());
	}

	public void setDataPolicy(DataPolicy dataPolicy) {
		this.dataPolicy = dataPolicy;
	}

	protected DataPolicy getDataPolicy() {
		return defaultIfNull(this.dataPolicy, DataPolicy.DEFAULT);
	}

	public void setKeyConstraint(Class<K> keyConstraint) {
		this.keyConstraint = keyConstraint;
	}

	protected Class<K> getKeyConstraint() {
		return this.keyConstraint;
	}

	public void setPoolName(String poolName) {
		this.poolName = poolName;
	}

	protected String getPoolName() {
		return defaultIfEmpty(this.poolName, GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME);
	}

	public void setServerRegionShortcut(RegionShortcut shortcut) {
		this.serverRegionShortcut = shortcut;
	}

	protected RegionShortcut getServerRegionShortcut() {
		return this.serverRegionShortcut;
	}

	public void setValueConstraint(Class<V> valueConstraint) {
		this.valueConstraint = valueConstraint;
	}

	protected Class<V> getValueConstraint() {
		return this.valueConstraint;
	}
}
