/*
 * Copyright 2016 the original author or authors.
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

import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.client.PoolManager;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.GenericRegionFactoryBean;
import org.springframework.data.gemfire.RegionLookupFactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.RegionConfigurer;
import org.springframework.util.StringUtils;

/**
 * The {@link GemFireCacheTypeAwareRegionFactoryBean} class is a smart Spring {@link FactoryBean} that knows how to
 * create a client or server {@link Region} depending on whether the {@link GemFireCache} is
 * a {@link org.apache.geode.cache.client.ClientCache} or a peer {@link org.apache.geode.cache.Cache}.
 *
 * @author John Blum
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.springframework.data.gemfire.GenericRegionFactoryBean
 * @see org.springframework.data.gemfire.RegionLookupFactoryBean
 * @see org.springframework.data.gemfire.RegionFactoryBean
 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
 * @since 1.9.0
 */
@SuppressWarnings("unused")
public class GemFireCacheTypeAwareRegionFactoryBean<K, V> extends RegionLookupFactoryBean<K, V> {

	private GemFireCache gemfireCache;

	private Boolean close = false;

	private Class<K> keyConstraint;
	private Class<V> valueConstraint;

	private ClientRegionShortcut clientRegionShortcut = ClientRegionShortcut.PROXY;

	private DataPolicy dataPolicy = DataPolicy.DEFAULT;

	private List<RegionConfigurer> regionConfigurers = Collections.emptyList();

	private RegionAttributes<K, V> regionAttributes;

	private RegionShortcut serverRegionShortcut;

	private String poolName;
	private String regionName;

	/**
	 * @inheritDoc
	 */
	@Override
	public Region<K, V> createRegion(GemFireCache gemfireCache, String regionName) throws Exception {

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
	 * @see org.apache.geode.cache.GemFireCache
	 * @see org.apache.geode.cache.Region
	 */
	protected Region<K, V> newClientRegion(GemFireCache gemfireCache, String regionName) throws Exception {

		ClientRegionFactoryBean<K, V> clientRegionFactory = new ClientRegionFactoryBean<>();

		clientRegionFactory.setAttributes(getRegionAttributes());
		clientRegionFactory.setBeanFactory(getBeanFactory());
		clientRegionFactory.setCache(gemfireCache);
		clientRegionFactory.setClose(isClose());
		clientRegionFactory.setKeyConstraint(getKeyConstraint());
		clientRegionFactory.setRegionConfigurers(this.regionConfigurers);
		clientRegionFactory.setRegionName(regionName);
		clientRegionFactory.setShortcut(getClientRegionShortcut());
		clientRegionFactory.setValueConstraint(getValueConstraint());

		resolvePoolName().ifPresent(clientRegionFactory::setPoolName);

		clientRegionFactory.afterPropertiesSet();

		return clientRegionFactory.getObject();
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
	 * @see org.apache.geode.cache.GemFireCache
	 * @see org.apache.geode.cache.Region
	 */
	protected Region<K, V> newServerRegion(GemFireCache gemfireCache, String regionName) throws Exception {

		GenericRegionFactoryBean<K, V> serverRegionFactory = new GenericRegionFactoryBean<>();

		serverRegionFactory.setAttributes(getRegionAttributes());
		serverRegionFactory.setBeanFactory(getBeanFactory());
		serverRegionFactory.setCache(gemfireCache);
		serverRegionFactory.setClose(isClose());
		serverRegionFactory.setDataPolicy(getDataPolicy());
		serverRegionFactory.setKeyConstraint(getKeyConstraint());
		serverRegionFactory.setRegionConfigurers(this.regionConfigurers);
		serverRegionFactory.setRegionName(regionName);
		serverRegionFactory.setShortcut(getServerRegionShortcut());
		serverRegionFactory.setValueConstraint(getValueConstraint());

		serverRegionFactory.afterPropertiesSet();

		return serverRegionFactory.getObject();
	}

	public void setAttributes(RegionAttributes<K, V> regionAttributes) {
		this.regionAttributes = regionAttributes;
	}

	protected RegionAttributes<K, V> getRegionAttributes() {
		return this.regionAttributes;
	}

	public void setClientRegionShortcut(ClientRegionShortcut clientRegionShortcut) {
		this.clientRegionShortcut = clientRegionShortcut;
	}

	protected ClientRegionShortcut getClientRegionShortcut() {
		return Optional.ofNullable(this.clientRegionShortcut).orElse(ClientRegionShortcut.PROXY);
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
		return Optional.ofNullable(this.dataPolicy).orElse(DataPolicy.DEFAULT);
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
		return Optional.ofNullable(this.poolName).filter(StringUtils::hasText)
			.orElse(ClientRegionFactoryBean.GEMFIRE_POOL_NAME);
	}

	protected Optional<String> resolvePoolName() {
		return Optional.of(getPoolName()).filter(this::isPoolResolvable);
	}

	private boolean isPoolResolvable(String poolName) {
		return (getBeanFactory().containsBean(poolName) || (PoolManager.find(poolName) != null));
	}

	/**
	 * Null-safe operation used to set an array of {@link RegionConfigurer RegionConfigurers} used to apply
	 * additional configuration to this {@link RegionLookupFactoryBean} when using Annotation-based configuration.
	 *
	 * @param regionConfigurers array of {@link RegionConfigurer RegionConfigurers} used to apply
	 * additional configuration to this {@link RegionLookupFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
	 * @see #setRegionConfigurers(List)
	 */
	public void setRegionConfigurers(RegionConfigurer... regionConfigurers) {
		setRegionConfigurers(Arrays.asList(nullSafeArray(regionConfigurers, RegionConfigurer.class)));
	}

	/**
	 * Null-safe operation used to set an {@link Iterable} of {@link RegionConfigurer RegionConfigurers} used to apply
	 * additional configuration to this {@link RegionLookupFactoryBean} when using Annotation-based configuration.
	 *
	 * @param regionConfigurers {@link Iterable} of {@link RegionConfigurer RegionConfigurers} used to apply
	 * additional configuration to this {@link RegionLookupFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
	 */
	public void setRegionConfigurers(List<RegionConfigurer> regionConfigurers) {
		this.regionConfigurers = Optional.ofNullable(regionConfigurers).orElseGet(Collections::emptyList);
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
