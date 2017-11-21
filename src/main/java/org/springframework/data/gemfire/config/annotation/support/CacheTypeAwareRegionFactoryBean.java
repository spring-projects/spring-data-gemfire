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

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.Scope;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.GenericRegionFactoryBean;
import org.springframework.data.gemfire.LocalRegionFactoryBean;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.data.gemfire.RegionLookupFactoryBean;
import org.springframework.data.gemfire.RegionShortcutWrapper;
import org.springframework.data.gemfire.ReplicatedRegionFactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.RegionConfigurer;
import org.springframework.util.StringUtils;

/**
 * The {@link CacheTypeAwareRegionFactoryBean} class is a smart Spring {@link FactoryBean} that knows how to
 * create a client or server {@link Region} depending on whether the {@link GemFireCache} is a {@link ClientCache}
 * or a peer {@link Cache}.
 *
 * @author John Blum
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.DataPolicy
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.RegionAttributes
 * @see org.apache.geode.cache.RegionShortcut
 * @see org.apache.geode.cache.Scope
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.client.ClientRegionShortcut
 * @see org.springframework.data.gemfire.GenericRegionFactoryBean
 * @see org.springframework.data.gemfire.LocalRegionFactoryBean
 * @see org.springframework.data.gemfire.PartitionedRegionFactoryBean
 * @see org.springframework.data.gemfire.RegionFactoryBean
 * @see org.springframework.data.gemfire.RegionLookupFactoryBean
 * @see org.springframework.data.gemfire.ReplicatedRegionFactoryBean
 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
 * @since 1.9.0
 */
@SuppressWarnings("unused")
public class CacheTypeAwareRegionFactoryBean<K, V> extends RegionLookupFactoryBean<K, V> {

	private GemFireCache gemfireCache;

	private Boolean close = false;
	private Boolean offHeap = false;

	private Class<K> keyConstraint;
	private Class<V> valueConstraint;

	private ClientRegionShortcut clientRegionShortcut = ClientRegionShortcut.PROXY;

	private DataPolicy dataPolicy = DataPolicy.DEFAULT;

	private List<RegionConfigurer> regionConfigurers = Collections.emptyList();

	private RegionAttributes<K, V> regionAttributes;

	private RegionShortcut serverRegionShortcut;

	private Scope scope;

	private String diskStoreName;
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
	 * Constructs, configures and initialize\s a new client {@link Region} using the {@link ClientRegionFactoryBean}.
	 *
	 * @param gemfireCache reference to the {@link GemFireCache} used to create/initialize the factory
	 * used to create the client {@link Region}.
	 * @param regionName name given to the client {@link Region}.
	 * @return a new instance of a client {@link Region} with the given {@code regionName}.
	 * @throws Exception if the client {@link Region} could not be created.
	 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
	 * @see org.apache.geode.cache.GemFireCache
	 * @see org.apache.geode.cache.Region
	 * @see #newClientRegionFactoryBean()
	 */
	protected Region<K, V> newClientRegion(GemFireCache gemfireCache, String regionName) throws Exception {

		ClientRegionFactoryBean<K, V> clientRegionFactory = newClientRegionFactoryBean();

		clientRegionFactory.setAttributes(getRegionAttributes());
		clientRegionFactory.setBeanFactory(getBeanFactory());
		clientRegionFactory.setCache(gemfireCache);
		clientRegionFactory.setClose(isClose());
		clientRegionFactory.setDiskStoreName(getDiskStoreName());
		clientRegionFactory.setKeyConstraint(getKeyConstraint());
		clientRegionFactory.setLookupEnabled(getLookupEnabled());
		clientRegionFactory.setRegionConfigurers(this.regionConfigurers);
		clientRegionFactory.setRegionName(regionName);
		clientRegionFactory.setShortcut(getClientRegionShortcut());
		clientRegionFactory.setValueConstraint(getValueConstraint());

		getPoolName().ifPresent(clientRegionFactory::setPoolName);

		clientRegionFactory.afterPropertiesSet();

		return clientRegionFactory.getObject();
	}

	/**
	 * Constructs a new instance of the {@link ClientRegionFactoryBean}.
	 *
	 * @param <K> {@link Class type} of the created {@link Region Region's} key.
	 * @param <V> {@link Class type} of the created {@link Region Region's} value.
	 * @return a new instance of the {@link ClientRegionFactoryBean}.
	 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
	 */
	protected <K, V> ClientRegionFactoryBean<K, V> newClientRegionFactoryBean() {
		return new ClientRegionFactoryBean<>();
	}

	/**
	 * Constructs, configures and initializes a new server {@link Region} using a sub-class
	 * of {@link RegionFactoryBean}.
	 *
	 * @param gemfireCache reference to the {@link GemFireCache} used to create/initialize the factory
	 * used to create the server {@link Region}.
	 * @param regionName name given to the server {@link Region}.
	 * @return a new instance of a server {@link Region} with the given {@code regionName}.
	 * @throws Exception if the server {@link Region} could not be created.
	 * @see org.springframework.data.gemfire.GenericRegionFactoryBean
	 * @see org.apache.geode.cache.GemFireCache
	 * @see org.apache.geode.cache.Region
	 * @see #newRegionFactoryBean()
	 */
	protected Region<K, V> newServerRegion(GemFireCache gemfireCache, String regionName) throws Exception {

		RegionFactoryBean<K, V> serverRegionFactory = newRegionFactoryBean();

		serverRegionFactory.setAttributes(getRegionAttributes());
		serverRegionFactory.setBeanFactory(getBeanFactory());
		serverRegionFactory.setCache(gemfireCache);
		serverRegionFactory.setClose(isClose());
		serverRegionFactory.setDataPolicy(getDataPolicy());
		serverRegionFactory.setDiskStoreName(getDiskStoreName());
		serverRegionFactory.setKeyConstraint(getKeyConstraint());
		serverRegionFactory.setLookupEnabled(getLookupEnabled());
		serverRegionFactory.setOffHeap(getOffHeap());
		serverRegionFactory.setRegionConfigurers(this.regionConfigurers);
		serverRegionFactory.setRegionName(regionName);
		serverRegionFactory.setShortcut(getServerRegionShortcut());
		serverRegionFactory.setValueConstraint(getValueConstraint());

		serverRegionFactory.afterPropertiesSet();

		return serverRegionFactory.getObject();
	}

	/**
	 * Constructs a {@link Class sub-type} of the {@link RegionFactoryBean} class based on
	 * the {@link #getServerRegionShortcut()} and {@link #getDataPolicy()}.
	 *
	 * @return a new instance of the {@link RegionFactoryBean}.
	 * @see org.springframework.data.gemfire.LocalRegionFactoryBean
	 * @see org.springframework.data.gemfire.PartitionedRegionFactoryBean
	 * @see org.springframework.data.gemfire.ReplicatedRegionFactoryBean
	 * @see org.springframework.data.gemfire.RegionFactoryBean
	 */
	protected RegionFactoryBean<K, V> newRegionFactoryBean() {

		RegionShortcutWrapper regionShortcutWrapper = RegionShortcutWrapper.valueOf(getServerRegionShortcut());

		DataPolicy resolvedDataPolicy = Optional.of(regionShortcutWrapper)
			.map(RegionShortcutWrapper::getDataPolicy)
			.orElseGet(this::getDataPolicy);

		if (regionShortcutWrapper.isLocal()) {
			return new LocalRegionFactoryBean<>();
		}
		else if (resolvedDataPolicy.withPartitioning()) {
			return new PartitionedRegionFactoryBean<>();
		}
		else if (resolvedDataPolicy.withReplication()) {
			ReplicatedRegionFactoryBean<K, V> replicatedRegionFactoryBean = new ReplicatedRegionFactoryBean<>();
			replicatedRegionFactoryBean.setScope(getScope());
			return replicatedRegionFactoryBean;
		}

		return new GenericRegionFactoryBean<>();
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

	public void setDiskStoreName(String diskStoreName) {
		this.diskStoreName = diskStoreName;
	}

	protected String getDiskStoreName() {
		return this.diskStoreName;
	}

	public void setKeyConstraint(Class<K> keyConstraint) {
		this.keyConstraint = keyConstraint;
	}

	protected Class<K> getKeyConstraint() {
		return this.keyConstraint;
	}

	public void setOffHeap(Boolean offHeap) {
		this.offHeap = offHeap;
	}

	protected Boolean getOffHeap() {
		return this.offHeap;
	}

	public void setPoolName(String poolName) {
		this.poolName = poolName;
	}

	protected Optional<String> getPoolName() {
		return Optional.ofNullable(this.poolName).filter(StringUtils::hasText);
	}

	protected String resolvePoolName() {
		return getPoolName().orElse(null);
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

	public void setScope(Scope scope) {
		this.scope = scope;
	}

	protected Scope getScope() {
		return this.scope;
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
