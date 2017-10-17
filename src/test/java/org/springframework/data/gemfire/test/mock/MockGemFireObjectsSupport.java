/*
 * Copyright 2017 the original author or authors.
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

package org.springframework.data.gemfire.test.mock;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyFloat;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.NOT_SUPPORTED;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newUnsupportedOperationException;

import java.io.File;
import java.io.InputStream;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheFactory;
import org.apache.geode.cache.CustomExpiry;
import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.DiskStore;
import org.apache.geode.cache.DiskStoreFactory;
import org.apache.geode.cache.EvictionAttributes;
import org.apache.geode.cache.ExpirationAction;
import org.apache.geode.cache.ExpirationAttributes;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.RegionService;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientCacheFactory;
import org.apache.geode.cache.client.ClientRegionFactory;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.client.PoolFactory;
import org.apache.geode.cache.control.ResourceManager;
import org.apache.geode.cache.execute.RegionFunctionContext;
import org.apache.geode.cache.query.CqAttributes;
import org.apache.geode.cache.query.CqQuery;
import org.apache.geode.cache.query.Query;
import org.apache.geode.cache.query.QueryService;
import org.apache.geode.cache.query.QueryStatistics;
import org.apache.geode.cache.server.CacheServer;
import org.apache.geode.cache.server.ClientSubscriptionConfig;
import org.apache.geode.compression.Compressor;
import org.apache.geode.distributed.DistributedSystem;
import org.apache.geode.internal.concurrent.ConcurrentHashSet;
import org.apache.geode.pdx.PdxSerializer;
import org.mockito.ArgumentMatchers;
import org.mockito.stubbing.Answer;
import org.springframework.data.gemfire.server.SubscriptionEvictionPolicy;
import org.springframework.data.gemfire.test.mock.support.MockObjectInvocationException;
import org.springframework.data.gemfire.test.support.FileSystemUtils;

/**
 * The {@link MockGemFireObjectsSupport} class is an abstract base class encapsulating factory methods for creating
 * Mock GemFire Objects (e.g. {@link Cache}, {@link ClientCache}, {@link Region}, etc).
 *
 * @author John Blum
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.CacheFactory
 * @see org.apache.geode.cache.DiskStore
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.client.ClientCacheFactory
 * @see org.apache.geode.cache.control.ResourceManager
 * @see org.apache.geode.cache.server.CacheServer
 * @see org.apache.geode.cache.server.ClientSubscriptionConfig
 * @see org.apache.geode.distributed.DistributedSystem
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.test.mock.MockObjectsSupport
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public abstract class MockGemFireObjectsSupport extends MockObjectsSupport {

	private static final boolean DEFAULT_USE_SINGLETON_CACHE = false;

	private static final AtomicReference<GemFireCache> singletonCache = new AtomicReference<>(null);

	private static final Map<String, DiskStore> diskStores = new ConcurrentHashMap<>();

	private static final Map<String, Region<Object, Object>> regions = new ConcurrentHashMap<>();

	private static final Map<String, RegionAttributes<Object, Object>> regionAttributes = new ConcurrentHashMap<>();

	private static final String FROM_KEYWORD = "FROM";
	private static final String WHERE_KEYWORD = "WHERE";

	private static final String REPEATING_REGION_SEPARATOR = Region.SEPARATOR + "{2,}";

	public static void destroy() {
		singletonCache.set(null);
		diskStores.clear();
		regions.clear();
		regionAttributes.clear();
	}

	/* (non-Javadoc) */
	private static boolean isRootRegion(Region<?, ?> region) {
		return isRootRegion(region.getFullPath());
	}

	/* (non-Javadoc) */
	private static boolean isRootRegion(String regionPath) {
		return (regionPath.lastIndexOf(Region.SEPARATOR) <= 0);
	}

	/* (non-Javadoc) */
	private static String normalizeRegionPath(String regionPath) {

		regionPath = regionPath.replaceAll(REPEATING_REGION_SEPARATOR, Region.SEPARATOR);
		regionPath = regionPath.endsWith(Region.SEPARATOR)
			? regionPath.substring(0, regionPath.length() - 1) : regionPath;

		return regionPath;
	}

	/* (non-Javadoc) */
	private static String toRegionName(String regionName) {

		return Optional.ofNullable(regionName)
			.map(String::trim)
			.map(it -> {
				int lastIndexOfRegionSeparator = it.lastIndexOf(Region.SEPARATOR);
				return lastIndexOfRegionSeparator < 0 ? it : it.substring(lastIndexOfRegionSeparator);
			})
			.filter(it -> !it.isEmpty())
			.orElseThrow(() -> newIllegalArgumentException("Region name [%s] is required", regionName));
	}

	/* (non-Javadoc) */
	private static String toRegionPath(String regionPath) {

		return Optional.ofNullable(regionPath)
			.map(String::trim)
			.map(it -> it.startsWith(Region.SEPARATOR) ? it : String.format("%1$s%2$s", Region.SEPARATOR, it))
			.map(MockGemFireObjectsSupport::normalizeRegionPath)
			.filter(it -> !it.isEmpty())
			.orElseThrow(() -> newIllegalArgumentException("Region path [%s] is required", regionPath));
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	private static <T extends GemFireCache> T mockCacheApi(T mockGemFireCache) {

		AtomicBoolean copyOnRead = new AtomicBoolean(false);

		DistributedSystem mockDistributedSystem = mockDistributedSystem();

		ResourceManager mockResourceManager = mockResourceManager();

		doAnswer(newSetter(copyOnRead, null)).when(mockGemFireCache).setCopyOnRead(anyBoolean());

		doAnswer(newSetter(regionAttributes, null))
			.when(mockGemFireCache).setRegionAttributes(anyString(), any(RegionAttributes.class));

		when(mockGemFireCache.getCopyOnRead()).thenAnswer(newGetter(copyOnRead));

		when(mockGemFireCache.getDistributedSystem()).thenReturn(mockDistributedSystem);

		when(mockGemFireCache.getRegionAttributes(anyString()))
			.thenAnswer(invocation -> regionAttributes.get(invocation.<String>getArgument(0)));

		when(mockGemFireCache.getResourceManager()).thenReturn(mockResourceManager);

		when(mockGemFireCache.createDiskStoreFactory()).thenAnswer(invocation -> mockDiskStoreFactory());

		when(mockGemFireCache.findDiskStore(anyString()))
			.thenAnswer(invocation -> diskStores.get(invocation.<String>getArgument(0)));

		when(mockGemFireCache.listRegionAttributes()).thenReturn(Collections.unmodifiableMap(regionAttributes));

		doThrow(newUnsupportedOperationException(NOT_SUPPORTED)).when(mockGemFireCache)
			.loadCacheXml(any(InputStream.class));

		return mockRegionServiceApi(mockGemFireCache);
	}

	/* (non-Javadoc) */
	private static <T extends RegionService> T mockRegionServiceApi(T mockRegionService) {

		AtomicBoolean closed = new AtomicBoolean(false);

		doAnswer(newSetter(closed, true, null)).when(mockRegionService).close();

		when(mockRegionService.isClosed()).thenAnswer(newGetter(closed));

		when(mockRegionService.getCancelCriterion()).thenThrow(newUnsupportedOperationException(NOT_SUPPORTED));

		when(mockRegionService.getRegion(anyString())).thenAnswer(invocation -> {

			String regionPath = invocation.getArgument(0);

			Optional.ofNullable(regionPath).map(String::trim).filter(it -> !it.isEmpty())
				.map(MockGemFireObjectsSupport::toRegionPath)
				.orElseThrow(() -> newIllegalArgumentException("Region path [%s] is not valid", regionPath));

			return regions.get(regionPath);
		});

		when(mockRegionService.createPdxEnum(anyString(), anyString(), anyInt()))
			.thenThrow(newUnsupportedOperationException(NOT_SUPPORTED));

		when(mockRegionService.createPdxInstanceFactory(anyString()))
			.thenThrow(newUnsupportedOperationException(NOT_SUPPORTED));

		when(mockRegionService.rootRegions()).thenAnswer(invocation ->
			regions.values().stream().filter(MockGemFireObjectsSupport::isRootRegion).collect(Collectors.toSet()));

		return mockRegionService;
	}

	public static ClientCache mockClientCache() {

		ClientCache mockClientCache = mock(ClientCache.class);

		doAnswer(newVoidAnswer(invocation -> mockClientCache.close())).when(mockClientCache).close(anyBoolean());

		when(mockClientCache.createClientRegionFactory(any(ClientRegionShortcut.class)))
			.thenAnswer(invocation -> mockClientRegionFactory(mockClientCache));

		return mockQueryService(mockCacheApi(mockClientCache));
	}

	public static GemFireCache mockGemFireCache() {

		GemFireCache mockGemFireCache = mock(GemFireCache.class);

		return mockQueryService(mockCacheApi(mockGemFireCache));
	}

	public static Cache mockPeerCache() {

		Cache mockCache = mock(Cache.class);

		AtomicInteger lockLease = new AtomicInteger();
		AtomicInteger lockTimeout = new AtomicInteger();
		AtomicInteger messageSyncInterval = new AtomicInteger();
		AtomicInteger searchTimeout = new AtomicInteger();

		List<CacheServer> cacheServers = new ArrayList<>();

		when(mockCache.addCacheServer()).thenAnswer(invocation -> {

			CacheServer mockCacheServer = mockCacheServer();

			cacheServers.add(mockCacheServer);

			return mockCacheServer;
		});

		doAnswer(newSetter(lockLease, null)).when(mockCache).setLockLease(anyInt());
		doAnswer(newSetter(lockTimeout, null)).when(mockCache).setLockTimeout(anyInt());
		doAnswer(newSetter(messageSyncInterval, null)).when(mockCache).setMessageSyncInterval(anyInt());
		doAnswer(newSetter(searchTimeout, null)).when(mockCache).setSearchTimeout(anyInt());

		when(mockCache.isServer()).thenReturn(true);
		when(mockCache.getCacheServers()).thenAnswer(invocation -> Collections.unmodifiableList(cacheServers));
		when(mockCache.getLockLease()).thenAnswer(newGetter(lockLease));
		when(mockCache.getLockTimeout()).thenAnswer(newGetter(lockTimeout));
		when(mockCache.getMessageSyncInterval()).thenAnswer(newGetter(messageSyncInterval));
		when(mockCache.getReconnectedCache()).thenAnswer(invocation -> mockPeerCache());
		when(mockCache.getSearchTimeout()).thenAnswer(newGetter(searchTimeout));

		return mockQueryService(mockCacheApi(mockCache));
	}

	public static CacheServer mockCacheServer() {

		CacheServer mockCacheServer = mock(CacheServer.class);

		AtomicBoolean tcpNoDelay = new AtomicBoolean(CacheServer.DEFAULT_TCP_NO_DELAY);

		AtomicInteger maxConnections = new AtomicInteger(CacheServer.DEFAULT_MAX_CONNECTIONS);
		AtomicInteger maxMessageCount = new AtomicInteger(CacheServer.DEFAULT_MAXIMUM_MESSAGE_COUNT);
		AtomicInteger maxThreads = new AtomicInteger(CacheServer.DEFAULT_MAX_THREADS);
		AtomicInteger maxTimeBetweenPings = new AtomicInteger(CacheServer.DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS);
		AtomicInteger messageTimeToLive = new AtomicInteger(CacheServer.DEFAULT_MESSAGE_TIME_TO_LIVE);
		AtomicInteger port = new AtomicInteger(CacheServer.DEFAULT_PORT);
		AtomicInteger socketBufferSize = new AtomicInteger(CacheServer.DEFAULT_SOCKET_BUFFER_SIZE);

		AtomicLong loadPollInterval = new AtomicLong(CacheServer.DEFAULT_LOAD_POLL_INTERVAL);

		AtomicReference<String> bindAddress = new AtomicReference<>(CacheServer.DEFAULT_BIND_ADDRESS);
		AtomicReference<String> hostnameForClients = new AtomicReference<>(CacheServer.DEFAULT_HOSTNAME_FOR_CLIENTS);

		doAnswer(newSetter(bindAddress, null))
			.when(mockCacheServer).setBindAddress(anyString());

		doAnswer(newSetter(hostnameForClients, null))
			.when(mockCacheServer).setHostnameForClients(anyString());

		doAnswer(newSetter(loadPollInterval, null))
			.when(mockCacheServer).setLoadPollInterval(anyLong());

		doAnswer(newSetter(maxConnections, null))
			.when(mockCacheServer).setMaxConnections(anyInt());

		doAnswer(newSetter(maxMessageCount, null))
			.when(mockCacheServer).setMaximumMessageCount(anyInt());

		doAnswer(newSetter(maxThreads, null))
			.when(mockCacheServer).setMaxThreads(anyInt());

		doAnswer(newSetter(maxTimeBetweenPings, null))
			.when(mockCacheServer).setMaximumTimeBetweenPings(anyInt());

		doAnswer(newSetter(messageTimeToLive, null))
			.when(mockCacheServer).setMessageTimeToLive(anyInt());

		doAnswer(newSetter(port, null))
			.when(mockCacheServer).setPort(anyInt());

		doAnswer(newSetter(socketBufferSize, null))
			.when(mockCacheServer).setSocketBufferSize(anyInt());

		doAnswer(newSetter(tcpNoDelay, null))
			.when(mockCacheServer).setTcpNoDelay(anyBoolean());

		when(mockCacheServer.getBindAddress()).thenAnswer(newGetter(bindAddress));
		when(mockCacheServer.getHostnameForClients()).thenAnswer(newGetter(hostnameForClients));
		when(mockCacheServer.getLoadPollInterval()).thenAnswer(newGetter(loadPollInterval));
		when(mockCacheServer.getMaxConnections()).thenAnswer(newGetter(maxConnections));
		when(mockCacheServer.getMaximumMessageCount()).thenAnswer(newGetter(maxMessageCount));
		when(mockCacheServer.getMaxThreads()).thenAnswer(newGetter(maxThreads));
		when(mockCacheServer.getMaximumTimeBetweenPings()).thenAnswer(newGetter(maxTimeBetweenPings));
		when(mockCacheServer.getMessageTimeToLive()).thenAnswer(newGetter(messageTimeToLive));
		when(mockCacheServer.getPort()).thenAnswer(newGetter(port));
		when(mockCacheServer.getSocketBufferSize()).thenAnswer(newGetter(socketBufferSize));
		when(mockCacheServer.getTcpNoDelay()).thenAnswer(newGetter(tcpNoDelay));

		ClientSubscriptionConfig mockClientSubsriptionConfig = mockClientSubscriptionConfig();

		when(mockCacheServer.getClientSubscriptionConfig()).thenReturn(mockClientSubsriptionConfig);

		return mockCacheServer;
	}

	@SuppressWarnings("unchecked")
	public static <K, V> ClientRegionFactory<K, V> mockClientRegionFactory(ClientCache mockClientCache) {

		ClientRegionFactory<K, V> mockClientRegionFactory =
			mock(ClientRegionFactory.class, mockObjectIdentifier("MockClientRegionFactory"));

		ExpirationAttributes DEFAULT_EXPIRATION_ATTRIBUTES =
			new ExpirationAttributes(0, ExpirationAction.INVALIDATE);

		AtomicBoolean cloningEnabled = new AtomicBoolean(false);
		AtomicBoolean concurrencyChecksEnabled = new AtomicBoolean(false);
		AtomicBoolean diskSynchronous = new AtomicBoolean(true);
		AtomicBoolean statisticsEnabled = new AtomicBoolean(false);

		AtomicInteger concurrencyLevel = new AtomicInteger(16);
		AtomicInteger initialCapacity = new AtomicInteger(16);

		AtomicReference<Compressor> compressor = new AtomicReference<>(null);
		AtomicReference<CustomExpiry<K, V>> customEntryIdleTimeout = new AtomicReference<>(null);
		AtomicReference<CustomExpiry<K, V>> customEntryTimeToLive = new AtomicReference<>(null);
		AtomicReference<String> diskStoreName = new AtomicReference<>(null);
		AtomicReference<ExpirationAttributes> entryIdleTimeout = new AtomicReference<>(DEFAULT_EXPIRATION_ATTRIBUTES);
		AtomicReference<ExpirationAttributes> entryTimeToLive = new AtomicReference<>(DEFAULT_EXPIRATION_ATTRIBUTES);
		AtomicReference<EvictionAttributes> evictionAttributes =
			new AtomicReference<>(EvictionAttributes.createLRUEntryAttributes());
		AtomicReference<Class<K>> keyConstraint = new AtomicReference<>();
		AtomicReference<Float> loadFactor = new AtomicReference<>(0.75f);
		AtomicReference<String> poolName = new AtomicReference<>(null);
		AtomicReference<ExpirationAttributes> regionIdleTimeout = new AtomicReference<>(DEFAULT_EXPIRATION_ATTRIBUTES);
		AtomicReference<ExpirationAttributes> regionTimeToLive = new AtomicReference<>(DEFAULT_EXPIRATION_ATTRIBUTES);
		AtomicReference<Class<K>> valueConstraint = new AtomicReference<>();

		when(mockClientRegionFactory.setCloningEnabled(anyBoolean()))
			.thenAnswer(newSetter(cloningEnabled, mockClientRegionFactory));

		when(mockClientRegionFactory.setCompressor(any(Compressor.class)))
			.thenAnswer(newSetter(compressor, mockClientRegionFactory));

		doAnswer(newSetter(concurrencyChecksEnabled, mockClientRegionFactory))
			.when(mockClientRegionFactory).setConcurrencyChecksEnabled(anyBoolean());

		when(mockClientRegionFactory.setConcurrencyLevel(anyInt()))
			.thenAnswer(newSetter(concurrencyLevel, mockClientRegionFactory));

		when(mockClientRegionFactory.setCustomEntryIdleTimeout(any(CustomExpiry.class)))
			.thenAnswer(newSetter(customEntryIdleTimeout, mockClientRegionFactory));

		when(mockClientRegionFactory.setCustomEntryTimeToLive(any(CustomExpiry.class)))
			.thenAnswer(newSetter(customEntryTimeToLive, mockClientRegionFactory));

		when(mockClientRegionFactory.setDiskStoreName(anyString()))
			.thenAnswer(newSetter(diskStoreName, mockClientRegionFactory));

		when(mockClientRegionFactory.setDiskSynchronous(anyBoolean()))
			.thenAnswer(newSetter(diskSynchronous, mockClientRegionFactory));

		when(mockClientRegionFactory.setEntryIdleTimeout(any(ExpirationAttributes.class)))
			.thenAnswer(newSetter(entryIdleTimeout, mockClientRegionFactory));

		when(mockClientRegionFactory.setEntryTimeToLive(any(ExpirationAttributes.class)))
			.thenAnswer(newSetter(entryTimeToLive, mockClientRegionFactory));

		when(mockClientRegionFactory.setEvictionAttributes(any(EvictionAttributes.class)))
			.thenAnswer(newSetter(evictionAttributes, mockClientRegionFactory));

		when(mockClientRegionFactory.setInitialCapacity(anyInt()))
			.thenAnswer(newSetter(initialCapacity, mockClientRegionFactory));

		when(mockClientRegionFactory.setKeyConstraint(any(Class.class)))
			.thenAnswer(newSetter(keyConstraint, mockClientRegionFactory));

		when(mockClientRegionFactory.setLoadFactor(anyFloat()))
			.thenAnswer(newSetter(loadFactor, mockClientRegionFactory));

		when(mockClientRegionFactory.setPoolName(anyString()))
			.thenAnswer(newSetter(poolName, mockClientRegionFactory));

		when(mockClientRegionFactory.setRegionIdleTimeout(any(ExpirationAttributes.class)))
			.thenAnswer(newSetter(regionIdleTimeout, mockClientRegionFactory));

		when(mockClientRegionFactory.setRegionTimeToLive(any(ExpirationAttributes.class)))
			.thenAnswer(newSetter(regionTimeToLive, mockClientRegionFactory));

		when(mockClientRegionFactory.setStatisticsEnabled(anyBoolean()))
			.thenAnswer(newSetter(statisticsEnabled, mockClientRegionFactory));

		when(mockClientRegionFactory.setValueConstraint(any(Class.class)))
			.thenAnswer(newSetter(valueConstraint, mockClientRegionFactory));

		RegionAttributes<K, V> mockRegionAttributes =
			mock(RegionAttributes.class, mockObjectIdentifier("MockRegionAttributes"));

		when(mockRegionAttributes.getCloningEnabled()).thenAnswer(newGetter(cloningEnabled));
		when(mockRegionAttributes.getCompressor()).thenAnswer(newGetter(compressor));
		when(mockRegionAttributes.getConcurrencyChecksEnabled()).thenAnswer(newGetter(concurrencyChecksEnabled));
		when(mockRegionAttributes.getConcurrencyLevel()).thenAnswer(newGetter(concurrencyLevel));
		when(mockRegionAttributes.getCustomEntryIdleTimeout()).thenAnswer(newGetter(customEntryIdleTimeout));
		when(mockRegionAttributes.getCustomEntryTimeToLive()).thenAnswer(newGetter(customEntryTimeToLive));
		when(mockRegionAttributes.getDataPolicy()).thenReturn(DataPolicy.NORMAL);
		when(mockRegionAttributes.getDiskStoreName()).thenAnswer(newGetter(diskStoreName));
		when(mockRegionAttributes.isDiskSynchronous()).thenAnswer(newGetter(diskSynchronous));
		when(mockRegionAttributes.getEntryIdleTimeout()).thenAnswer(newGetter(entryIdleTimeout));
		when(mockRegionAttributes.getEntryTimeToLive()).thenAnswer(newGetter(entryTimeToLive));
		when(mockRegionAttributes.getEvictionAttributes()).thenAnswer(newGetter(evictionAttributes));
		when(mockRegionAttributes.getInitialCapacity()).thenAnswer(newGetter(initialCapacity));
		when(mockRegionAttributes.getKeyConstraint()).thenAnswer(newGetter(keyConstraint));
		when(mockRegionAttributes.getLoadFactor()).thenAnswer(newGetter(loadFactor));
		when(mockRegionAttributes.getPoolName()).thenAnswer(newGetter(poolName));
		when(mockRegionAttributes.getRegionIdleTimeout()).thenAnswer(newGetter(regionIdleTimeout));
		when(mockRegionAttributes.getRegionTimeToLive()).thenAnswer(newGetter(regionTimeToLive));
		when(mockRegionAttributes.getStatisticsEnabled()).thenAnswer(newGetter(statisticsEnabled));
		when(mockRegionAttributes.getValueConstraint()).thenAnswer(newGetter(valueConstraint));

		when(mockClientRegionFactory.create(anyString())).thenAnswer(invocation ->
			mockRegion(mockClientCache, invocation.getArgument(0), mockRegionAttributes));

		when(mockClientRegionFactory.createSubregion(any(Region.class), anyString())).thenAnswer(invocation ->
			mockSubRegion(invocation.getArgument(0), invocation.getArgument(1), mockRegionAttributes));

		return mockClientRegionFactory;
	}

	public static ClientSubscriptionConfig mockClientSubscriptionConfig() {

		ClientSubscriptionConfig mockClientSubscriptionConfig = mock(ClientSubscriptionConfig.class);

		AtomicInteger subscriptionCapacity = new AtomicInteger(ClientSubscriptionConfig.DEFAULT_CAPACITY);

		AtomicReference<String> subscriptionDiskStoreName = new AtomicReference<>("");

		AtomicReference<SubscriptionEvictionPolicy> subscriptionEvictionPolicy =
			new AtomicReference<>(SubscriptionEvictionPolicy.DEFAULT);

		Function<String, SubscriptionEvictionPolicy> stringToSubscriptionEvictionPolicyConverter =
			arg -> SubscriptionEvictionPolicy.valueOfIgnoreCase(String.valueOf(arg));

		Function<SubscriptionEvictionPolicy, String> subscriptionEvictionPolicyToStringConverter =
			arg -> Optional.ofNullable(arg).map(Object::toString).map(String::toLowerCase).orElse(null);

		doAnswer(newSetter(subscriptionCapacity, null))
			.when(mockClientSubscriptionConfig).setCapacity(anyInt());

		doAnswer(newSetter(subscriptionDiskStoreName, null))
			.when(mockClientSubscriptionConfig).setDiskStoreName(anyString());

		doAnswer(newSetter(subscriptionEvictionPolicy, stringToSubscriptionEvictionPolicyConverter, null))
			.when(mockClientSubscriptionConfig).setEvictionPolicy(anyString());

		when(mockClientSubscriptionConfig.getCapacity()).thenAnswer(newGetter(subscriptionCapacity));
		when(mockClientSubscriptionConfig.getDiskStoreName()).thenAnswer(newGetter(subscriptionDiskStoreName));
		when(mockClientSubscriptionConfig.getEvictionPolicy()).thenAnswer(newGetter(subscriptionEvictionPolicy,
			subscriptionEvictionPolicyToStringConverter));

		return mockClientSubscriptionConfig;
	}

	public static DiskStoreFactory mockDiskStoreFactory() {

		DiskStoreFactory mockDiskStoreFactory = mock(DiskStoreFactory.class);

		AtomicBoolean allowForceCompaction = new AtomicBoolean(DiskStoreFactory.DEFAULT_ALLOW_FORCE_COMPACTION);
		AtomicBoolean autoCompact = new AtomicBoolean(DiskStoreFactory.DEFAULT_AUTO_COMPACT);

		AtomicInteger compactionThreshold = new AtomicInteger(DiskStoreFactory.DEFAULT_COMPACTION_THRESHOLD);
		AtomicInteger queueSize = new AtomicInteger(DiskStoreFactory.DEFAULT_QUEUE_SIZE);
		AtomicInteger writeBufferSize = new AtomicInteger(DiskStoreFactory.DEFAULT_WRITE_BUFFER_SIZE);

		AtomicLong maxOplogSize = new AtomicLong(DiskStoreFactory.DEFAULT_MAX_OPLOG_SIZE);
		AtomicLong timeInterval = new AtomicLong(DiskStoreFactory.DEFAULT_TIME_INTERVAL);

		AtomicReference<File[]> diskDirectories =
			new AtomicReference<>(new File[] { FileSystemUtils.WORKING_DIRECTORY });

		AtomicReference<int[]> diskDiretorySizes = new AtomicReference<>(new int[0]);

		AtomicReference<Float> diskUsageCriticalPercentage =
			new AtomicReference<>(DiskStoreFactory.DEFAULT_DISK_USAGE_CRITICAL_PERCENTAGE);

		AtomicReference<Float> diskUsageWarningPercentage =
			new AtomicReference<>(DiskStoreFactory.DEFAULT_DISK_USAGE_WARNING_PERCENTAGE);

		when(mockDiskStoreFactory.setAllowForceCompaction(anyBoolean()))
			.thenAnswer(newSetter(allowForceCompaction, mockDiskStoreFactory));

		when(mockDiskStoreFactory.setAutoCompact(anyBoolean()))
			.thenAnswer(newSetter(autoCompact, mockDiskStoreFactory));

		when(mockDiskStoreFactory.setCompactionThreshold(anyInt()))
			.thenAnswer(newSetter(compactionThreshold, mockDiskStoreFactory));

		when(mockDiskStoreFactory.setDiskDirs(any(File[].class))).thenAnswer(invocation -> {

			File[] resolveDiskDirectories = nullSafeArray(invocation.getArgument(0), File.class);

			int[] resolvedDiskDirectorySizes = new int[resolveDiskDirectories.length];

			Arrays.fill(resolvedDiskDirectorySizes, DiskStoreFactory.DEFAULT_DISK_DIR_SIZE);

			diskDirectories.set(resolveDiskDirectories);
			diskDiretorySizes.set(resolvedDiskDirectorySizes);

			return mockDiskStoreFactory;
		});

		when(mockDiskStoreFactory.setDiskDirsAndSizes(any(File[].class), any(int[].class))).thenAnswer(invocation -> {

			diskDirectories.set(invocation.getArgument(0));
			diskDiretorySizes.set(invocation.getArgument(1));

			return mockDiskStoreFactory;
		});

		when(mockDiskStoreFactory.setDiskUsageCriticalPercentage(anyFloat()))
			.thenAnswer(newSetter(diskUsageCriticalPercentage, mockDiskStoreFactory));

		when(mockDiskStoreFactory.setDiskUsageWarningPercentage(anyFloat()))
			.thenAnswer(newSetter(diskUsageWarningPercentage, mockDiskStoreFactory));

		when(mockDiskStoreFactory.setMaxOplogSize(anyLong()))
			.thenAnswer(newSetter(maxOplogSize, mockDiskStoreFactory));

		when(mockDiskStoreFactory.setQueueSize(anyInt()))
			.thenAnswer(newSetter(queueSize, mockDiskStoreFactory));

		when(mockDiskStoreFactory.setTimeInterval(anyLong()))
			.thenAnswer(newSetter(timeInterval, mockDiskStoreFactory));

		when(mockDiskStoreFactory.setWriteBufferSize(anyInt()))
			.thenAnswer(newSetter(writeBufferSize, mockDiskStoreFactory));

		when(mockDiskStoreFactory.create(anyString())).thenAnswer(invocation -> {

			String name = invocation.getArgument(0);

			DiskStore mockDiskStore = mock(DiskStore.class, name);

			when(mockDiskStore.getAllowForceCompaction()).thenReturn(allowForceCompaction.get());
			when(mockDiskStore.getAutoCompact()).thenReturn(autoCompact.get());
			when(mockDiskStore.getCompactionThreshold()).thenReturn(compactionThreshold.get());
			when(mockDiskStore.getDiskDirs()).thenReturn(diskDirectories.get());
			when(mockDiskStore.getDiskDirSizes()).thenReturn(diskDiretorySizes.get());
			when(mockDiskStore.getDiskUsageCriticalPercentage()).thenReturn(diskUsageCriticalPercentage.get());
			when(mockDiskStore.getDiskUsageWarningPercentage()).thenReturn(diskUsageWarningPercentage.get());
			when(mockDiskStore.getDiskStoreUUID()).thenReturn(UUID.randomUUID());
			when(mockDiskStore.getMaxOplogSize()).thenReturn(maxOplogSize.get());
			when(mockDiskStore.getName()).thenReturn(name);
			when(mockDiskStore.getQueueSize()).thenReturn(queueSize.get());
			when(mockDiskStore.getTimeInterval()).thenReturn(timeInterval.get());
			when(mockDiskStore.getWriteBufferSize()).thenReturn(writeBufferSize.get());

			diskStores.put(name, mockDiskStore);

			return mockDiskStore;
		});

		return mockDiskStoreFactory;
	}

	public static DistributedSystem mockDistributedSystem() {

		DistributedSystem mockDistributedSystem = mock(DistributedSystem.class);

		when(mockDistributedSystem.getProperties()).thenReturn(new Properties());
		when(mockDistributedSystem.getReconnectedSystem()).thenAnswer(invocation -> mockDistributedSystem());

		return mockDistributedSystem;
	}

	public static PoolFactory mockPoolFactory() {

		PoolFactory mockPoolFactory = mock(PoolFactory.class);

		AtomicBoolean multiuserAuthentication = new AtomicBoolean(PoolFactory.DEFAULT_MULTIUSER_AUTHENTICATION);
		AtomicBoolean prSingleHopEnabled = new AtomicBoolean(PoolFactory.DEFAULT_PR_SINGLE_HOP_ENABLED);
		AtomicBoolean subscriptionEnabled = new AtomicBoolean(PoolFactory.DEFAULT_SUBSCRIPTION_ENABLED);
		AtomicBoolean threadLocalConnections = new AtomicBoolean(PoolFactory.DEFAULT_THREAD_LOCAL_CONNECTIONS);

		AtomicInteger freeConnectionTimeout = new AtomicInteger(PoolFactory.DEFAULT_FREE_CONNECTION_TIMEOUT);
		AtomicInteger loadConditioningInterval = new AtomicInteger(PoolFactory.DEFAULT_LOAD_CONDITIONING_INTERVAL);
		AtomicInteger maxConnections = new AtomicInteger(PoolFactory.DEFAULT_MAX_CONNECTIONS);
		AtomicInteger minConnections = new AtomicInteger(PoolFactory.DEFAULT_MIN_CONNECTIONS);
		AtomicInteger readTimeout = new AtomicInteger(PoolFactory.DEFAULT_READ_TIMEOUT);
		AtomicInteger retryAttempts = new AtomicInteger(PoolFactory.DEFAULT_RETRY_ATTEMPTS);
		AtomicInteger socketBufferSize = new AtomicInteger(PoolFactory.DEFAULT_SOCKET_BUFFER_SIZE);
		AtomicInteger statisticInterval = new AtomicInteger(PoolFactory.DEFAULT_STATISTIC_INTERVAL);
		AtomicInteger subscriptionAckInterval = new AtomicInteger(PoolFactory.DEFAULT_SUBSCRIPTION_ACK_INTERVAL);
		AtomicInteger subscriptionMessageTrackingTimeout = new AtomicInteger(PoolFactory.DEFAULT_SUBSCRIPTION_MESSAGE_TRACKING_TIMEOUT);
		AtomicInteger subscriptionRedundancy = new AtomicInteger(PoolFactory.DEFAULT_SUBSCRIPTION_REDUNDANCY);

		AtomicLong idleTimeout = new AtomicLong(PoolFactory.DEFAULT_IDLE_TIMEOUT);
		AtomicLong pingInterval = new AtomicLong(PoolFactory.DEFAULT_PING_INTERVAL);

		AtomicReference<String> serverGroup = new AtomicReference<>(PoolFactory.DEFAULT_SERVER_GROUP);

		List<InetSocketAddress> locators = new ArrayList<>();
		List<InetSocketAddress> servers = new ArrayList<>();

		when(mockPoolFactory.addLocator(anyString(), anyInt())).thenAnswer(invocation -> {
			locators.add(new InetSocketAddress(invocation.<String>getArgument(0), invocation.getArgument(1)));
			return mockPoolFactory;
		});

		when(mockPoolFactory.addServer(anyString(), anyInt())).thenAnswer(invocation -> {
			servers.add(new InetSocketAddress(invocation.<String>getArgument(0), invocation.getArgument(1)));
			return mockPoolFactory;
		});

		when(mockPoolFactory.setFreeConnectionTimeout(anyInt()))
			.thenAnswer(newSetter(freeConnectionTimeout, mockPoolFactory));

		when(mockPoolFactory.setIdleTimeout(anyLong()))
			.thenAnswer(newSetter(idleTimeout, mockPoolFactory));

		when(mockPoolFactory.setLoadConditioningInterval(anyInt()))
			.thenAnswer(newSetter(loadConditioningInterval, mockPoolFactory));

		when(mockPoolFactory.setMaxConnections(anyInt()))
			.thenAnswer(newSetter(maxConnections, mockPoolFactory));

		when(mockPoolFactory.setMinConnections(anyInt()))
			.thenAnswer(newSetter(minConnections, mockPoolFactory));

		when(mockPoolFactory.setMultiuserAuthentication(anyBoolean()))
			.thenAnswer(newSetter(multiuserAuthentication, mockPoolFactory));

		when(mockPoolFactory.setPingInterval(anyLong()))
			.thenAnswer(newSetter(pingInterval, mockPoolFactory));

		when(mockPoolFactory.setPRSingleHopEnabled(anyBoolean()))
			.thenAnswer(newSetter(prSingleHopEnabled, mockPoolFactory));

		when(mockPoolFactory.setReadTimeout(anyInt()))
			.thenAnswer(newSetter(readTimeout, mockPoolFactory));

		when(mockPoolFactory.setRetryAttempts(anyInt()))
			.thenAnswer(newSetter(retryAttempts, mockPoolFactory));

		when(mockPoolFactory.setServerGroup(anyString()))
			.thenAnswer(newSetter(serverGroup, mockPoolFactory));

		when(mockPoolFactory.setSocketBufferSize(anyInt()))
			.thenAnswer(newSetter(socketBufferSize, mockPoolFactory));

		when(mockPoolFactory.setStatisticInterval(anyInt()))
			.thenAnswer(newSetter(statisticInterval, mockPoolFactory));

		when(mockPoolFactory.setSubscriptionAckInterval(anyInt()))
			.thenAnswer(newSetter(subscriptionAckInterval, mockPoolFactory));

		when(mockPoolFactory.setSubscriptionEnabled(anyBoolean()))
			.thenAnswer(newSetter(subscriptionEnabled, mockPoolFactory));

		when(mockPoolFactory.setSubscriptionMessageTrackingTimeout(anyInt()))
			.thenAnswer(newSetter(subscriptionMessageTrackingTimeout, mockPoolFactory));

		when(mockPoolFactory.setSubscriptionRedundancy(anyInt()))
			.thenAnswer(newSetter(subscriptionRedundancy, mockPoolFactory));

		when(mockPoolFactory.setThreadLocalConnections(anyBoolean()))
			.thenAnswer(newSetter(threadLocalConnections, mockPoolFactory));

		when(mockPoolFactory.create(anyString())).thenAnswer(invocation -> {

			String name = invocation.getArgument(0);

			Pool mockPool = mock(Pool.class, name);

			AtomicBoolean destroyed = new AtomicBoolean(false);

			doAnswer(invocationOnMock -> {
				destroyed.set(true);
				return null;
			}).when(mockPool).destroy();

			doAnswer(invocationOnMock -> {
				destroyed.set(true);
				return null;
			}).when(mockPool).destroy(anyBoolean());

			when(mockPool.isDestroyed()).thenAnswer(newGetter(destroyed));
			when(mockPool.getFreeConnectionTimeout()).thenReturn(freeConnectionTimeout.get());
			when(mockPool.getIdleTimeout()).thenReturn(idleTimeout.get());
			when(mockPool.getLoadConditioningInterval()).thenReturn(loadConditioningInterval.get());
			when(mockPool.getLocators()).thenReturn(locators);
			when(mockPool.getMaxConnections()).thenReturn(maxConnections.get());
			when(mockPool.getMinConnections()).thenReturn(minConnections.get());
			when(mockPool.getMultiuserAuthentication()).thenReturn(multiuserAuthentication.get());
			when(mockPool.getName()).thenReturn(name);
			when(mockPool.getPingInterval()).thenReturn(pingInterval.get());
			when(mockPool.getPRSingleHopEnabled()).thenReturn(prSingleHopEnabled.get());
			when(mockPool.getReadTimeout()).thenReturn(readTimeout.get());
			when(mockPool.getRetryAttempts()).thenReturn(retryAttempts.get());
			when(mockPool.getServerGroup()).thenReturn(serverGroup.get());
			when(mockPool.getServers()).thenReturn(servers);
			when(mockPool.getSocketBufferSize()).thenReturn(socketBufferSize.get());
			when(mockPool.getStatisticInterval()).thenReturn(statisticInterval.get());
			when(mockPool.getSubscriptionAckInterval()).thenReturn(subscriptionAckInterval.get());
			when(mockPool.getSubscriptionEnabled()).thenReturn(subscriptionEnabled.get());
			when(mockPool.getSubscriptionMessageTrackingTimeout()).thenReturn(subscriptionMessageTrackingTimeout.get());
			when(mockPool.getSubscriptionRedundancy()).thenReturn(subscriptionRedundancy.get());
			when(mockPool.getThreadLocalConnections()).thenReturn(threadLocalConnections.get());

			return mockPool;
		});

		return mockPoolFactory;
	}

	public static Pool mockQueryService(Pool pool) {

		QueryService mockQueryService = mockQueryService();

		when(pool.getQueryService()).thenReturn(mockQueryService);

		return pool;
	}

	public static <T extends RegionService> T mockQueryService(T regionService) {

		QueryService mockQueryService = mockQueryService();

		when(regionService.getQueryService()).thenReturn(mockQueryService);

		if (regionService instanceof ClientCache) {
			when(((ClientCache) regionService).getLocalQueryService()).thenReturn(mockQueryService);
		}

		return regionService;
	}

	// TODO write more mocking logic for the QueryService interface
	public static QueryService mockQueryService() {

		QueryService mockQueryService = mock(QueryService.class);

		Set<CqQuery> cqQueries = new ConcurrentHashSet<>();

		try {
			when(mockQueryService.getCqs()).thenAnswer(invocation -> cqQueries.toArray(new CqQuery[cqQueries.size()]));

			when(mockQueryService.getCq(anyString())).thenAnswer(invocation ->
				cqQueries.stream().filter(cqQuery -> invocation.getArgument(0).equals(cqQuery.getName()))
					.findFirst().orElse(null));

			when(mockQueryService.getCqs(anyString())).thenAnswer(invocation -> {

				List<CqQuery> cqQueriesByRegion = cqQueries.stream().filter(cqQuery -> {

					String queryString = cqQuery.getQueryString();

					int indexOfFromClause = queryString.indexOf(FROM_KEYWORD);
					int indexOfWhereClause = queryString.indexOf(WHERE_KEYWORD);

					queryString = (indexOfFromClause > -1
						? queryString.substring(indexOfFromClause + FROM_KEYWORD.length()) : queryString);

					queryString = (indexOfWhereClause > 0 ? queryString.substring(0, indexOfWhereClause) : queryString);

					queryString = (queryString.startsWith(Region.SEPARATOR) ? queryString.substring(1) : queryString);

					return invocation.getArgument(0).equals(queryString.trim());

				}).collect(Collectors.toList());

				return cqQueriesByRegion.toArray(new CqQuery[cqQueriesByRegion.size()]);
			});

			when(mockQueryService.newCq(anyString(), any(CqAttributes.class))).thenAnswer(invocation ->
				add(cqQueries, mockCqQuery(null, invocation.getArgument(0), invocation.getArgument(1),
					false)));

			when(mockQueryService.newCq(anyString(), any(CqAttributes.class), anyBoolean())).thenAnswer(invocation ->
				add(cqQueries, mockCqQuery(null, invocation.getArgument(0), invocation.getArgument(1),
					invocation.getArgument(2))));

			when(mockQueryService.newCq(anyString(), anyString(), any(CqAttributes.class))).thenAnswer(invocation ->
				add(cqQueries, mockCqQuery(invocation.getArgument(0), invocation.getArgument(1),
					invocation.getArgument(2), false)));

			when(mockQueryService.newCq(anyString(), anyString(), any(CqAttributes.class), anyBoolean()))
				.thenAnswer(invocation -> add(cqQueries, mockCqQuery(invocation.getArgument(0),
					invocation.getArgument(1), invocation.getArgument(2), invocation.getArgument(3))));
		}
		catch (Exception cause) {
			throw new MockObjectInvocationException(cause);
		}

		return mockQueryService;
	}

	private static CqQuery add(Collection<CqQuery> cqQueries, CqQuery cqQuery) {
		cqQueries.add(cqQuery);
		return cqQuery;
	}

	private static CqQuery mockCqQuery(String name, String queryString, CqAttributes cqAttributes, boolean durable) {

		CqQuery mockCqQuery = mock(CqQuery.class);

		Query mockQuery = mockQuery(queryString);

		AtomicBoolean closed = new AtomicBoolean(false);
		AtomicBoolean running = new AtomicBoolean(false);
		AtomicBoolean stopped = new AtomicBoolean(true);

		when(mockCqQuery.getCqAttributes()).thenReturn(cqAttributes);
		when(mockCqQuery.getName()).thenReturn(name);
		when(mockCqQuery.getQuery()).thenReturn(mockQuery);
		when(mockCqQuery.getQueryString()).thenReturn(queryString);

		try {
			doAnswer(newSetter(closed, true, null)).when(mockCqQuery).close();

			doAnswer(invocation -> {

				running.set(true);
				stopped.set(false);

				return null;

			}).when(mockCqQuery).execute();

			doAnswer(invocation -> {

				running.set(false);
				stopped.set(true);

				return null;

			}).when(mockCqQuery).stop();
		}
		catch (Exception cause) {
			throw new MockObjectInvocationException(cause);
		}

		when(mockCqQuery.isClosed()).thenAnswer(newGetter(closed));
		when(mockCqQuery.isDurable()).thenReturn(durable);
		when(mockCqQuery.isRunning()).thenAnswer(newGetter(running));
		when(mockCqQuery.isStopped()).thenAnswer(newGetter(stopped));

		return mockCqQuery;
	}

	private static Query mockQuery(String queryString) {

		Query mockQuery = mock(Query.class);

		QueryStatistics mockQueryStatistics = mockQueryStatistics(mockQuery);

		when(mockQuery.getQueryString()).thenReturn(queryString);
		when(mockQuery.getStatistics()).thenReturn(mockQueryStatistics);

		return mockQuery;
	}

	private static QueryStatistics mockQueryStatistics(Query query) {

		QueryStatistics mockQueryStatistics = mock(QueryStatistics.class);

		AtomicLong numberOfExecutions = new AtomicLong(0L);

		Answer<Object> executeAnswer = invocation -> {
			numberOfExecutions.incrementAndGet();
			return null;
		};

		try {
			when(query.execute()).thenAnswer(executeAnswer);
			when(query.execute(any(Object[].class))).thenAnswer(executeAnswer);
			when(query.execute(any(RegionFunctionContext.class))).thenAnswer(executeAnswer);
			when(query.execute(any(RegionFunctionContext.class), any(Object[].class))).thenAnswer(executeAnswer);
		}
		catch (Exception cause) {
			throw new MockObjectInvocationException(cause);
		}

		when(mockQueryStatistics.getNumExecutions()).thenAnswer(newGetter(numberOfExecutions));
		when(mockQueryStatistics.getTotalExecutionTime()).thenReturn(0L);

		return mockQueryStatistics;
	}

	@SuppressWarnings("unchecked")
	public static <K, V> Region<K, V> mockRegion(RegionService regionService, String name,
			RegionAttributes<K, V> regionAttributes) {

		Map<K, V> data = new ConcurrentHashMap<>();

		Region<K, V> mockRegion = mock(Region.class, name);

		Set<Region<?, ?>> subRegions = new CopyOnWriteArraySet<>();

		when(mockRegion.getAttributes()).thenReturn(regionAttributes);
		when(mockRegion.getFullPath()).thenReturn(toRegionPath(name));
		when(mockRegion.getName()).thenReturn(toRegionName(name));
		when(mockRegion.getRegionService()).thenReturn(regionService);

		when(mockRegion.getSubregion(anyString())).thenAnswer(invocation -> {

			String subRegionPath = toRegionPath(invocation.getArgument(0));
			String subRegionFullPath = String.format("%1$s%2$s", mockRegion.getFullPath(), subRegionPath);

			return regions.get(subRegionFullPath);
		});

		when(mockRegion.get(ArgumentMatchers.<K>any())).thenAnswer(invocation ->
			data.get(invocation.<K>getArgument(0)));

		when(mockRegion.getEntry(ArgumentMatchers.<K>any())).thenAnswer(invocation ->
			data.entrySet().stream().filter(entry -> entry.getKey().equals(invocation.getArgument(0))).findFirst());

		when(mockRegion.put(any(), any())).thenAnswer(invocation ->
			data.put(invocation.getArgument(0), invocation.getArgument(1)));

		when(mockRegion.size()).thenAnswer(invocation -> data.size());

		when(mockRegion.subregions(anyBoolean())).thenAnswer(invocation -> {

			boolean recursive = invocation.getArgument(0);

			return recursive ? subRegions.stream()
					.flatMap(subRegion -> subRegion.subregions(true).stream()).collect(Collectors.toSet())
				: subRegions;
		});

		regions.put(mockRegion.getFullPath(), (Region) mockRegion);

		return mockRegion;
	}

	public static <K, V> Region<K, V> mockSubRegion(Region<K, V> parent, String name,
			RegionAttributes<K, V> regionAttributes) {

		String subRegionName = String.format("%1$s%2$s", parent.getFullPath(), toRegionPath(name));

		Region<K, V> mockSubRegion = mockRegion(parent.getRegionService(), subRegionName, regionAttributes);

		parent.subregions(false).add(mockSubRegion);

		return mockSubRegion;
	}

	public static ResourceManager mockResourceManager() {

		ResourceManager mockResourceManager = mock(ResourceManager.class);

		AtomicReference<Float> criticalHeapPercentage =
			new AtomicReference<>(ResourceManager.DEFAULT_CRITICAL_PERCENTAGE);

		AtomicReference<Float> criticalOffHeapPercentage =
			new AtomicReference<>(ResourceManager.DEFAULT_CRITICAL_PERCENTAGE);

		AtomicReference<Float> evictionHeapPercentage =
			new AtomicReference<>(ResourceManager.DEFAULT_EVICTION_PERCENTAGE);

		AtomicReference<Float> evictionOffHeapPercentage =
			new AtomicReference<>(ResourceManager.DEFAULT_EVICTION_PERCENTAGE);

		doAnswer(newSetter(criticalHeapPercentage, null))
			.when(mockResourceManager).setCriticalHeapPercentage(anyFloat());

		doAnswer(newSetter(criticalOffHeapPercentage, null))
			.when(mockResourceManager).setCriticalOffHeapPercentage(anyFloat());

		doAnswer(newSetter(evictionHeapPercentage, null))
			.when(mockResourceManager).setEvictionHeapPercentage(anyFloat());

		doAnswer(newSetter(evictionOffHeapPercentage, null))
			.when(mockResourceManager).setEvictionOffHeapPercentage(anyFloat());

		when(mockResourceManager.getCriticalHeapPercentage()).thenAnswer(newGetter(criticalHeapPercentage));
		when(mockResourceManager.getCriticalOffHeapPercentage()).thenAnswer(newGetter(criticalOffHeapPercentage));
		when(mockResourceManager.getEvictionHeapPercentage()).thenAnswer(newGetter(evictionHeapPercentage));
		when(mockResourceManager.getEvictionOffHeapPercentage()).thenAnswer(newGetter(evictionOffHeapPercentage));
		when(mockResourceManager.getRebalanceOperations()).thenReturn(Collections.emptySet());

		return mockResourceManager;
	}

	private static <T extends GemFireCache> T rememberMockedGemFireCache(T mockedGemFireCache,
			boolean useSingletonCache) {

		return Optional.ofNullable(mockedGemFireCache)
			.map(it -> {
				if (useSingletonCache) {
					singletonCache.compareAndSet(null, mockedGemFireCache);
				}

				return mockedGemFireCache;
			})
			.orElseThrow(() -> newIllegalArgumentException("GemFireCache is required"));
	}

	@SuppressWarnings("unchecked")
	private static <T extends GemFireCache> Optional<T> resolveMockedGemFireCache(boolean useSingletonCache) {
		return Optional.ofNullable((T) singletonCache.get()).filter(it -> useSingletonCache);
	}

	public static CacheFactory spyOn(CacheFactory cacheFactory) {
		return spyOn(cacheFactory, DEFAULT_USE_SINGLETON_CACHE);
	}

	public static CacheFactory spyOn(CacheFactory cacheFactory, boolean useSingletonCache) {

		CacheFactory cacheFactorySpy = spy(cacheFactory);

		Cache resolvedMockCache = MockGemFireObjectsSupport.<Cache>resolveMockedGemFireCache(useSingletonCache)
			.orElseGet(() -> {

				Cache mockCache = mockPeerCache();

				AtomicBoolean pdxIgnoreUnreadFields = new AtomicBoolean(false);
				AtomicBoolean pdxPersistent = new AtomicBoolean(false);
				AtomicBoolean pdxReadSerialized = new AtomicBoolean(false);

				AtomicReference<String> pdxDiskStoreName = new AtomicReference<>(null);
				AtomicReference<PdxSerializer> pdxSerializer = new AtomicReference<>(null);

				doAnswer(newSetter(pdxDiskStoreName, cacheFactorySpy))
					.when(cacheFactorySpy).setPdxDiskStore(anyString());

				doAnswer(newSetter(pdxIgnoreUnreadFields, cacheFactorySpy))
					.when(cacheFactorySpy).setPdxIgnoreUnreadFields(anyBoolean());

				doAnswer(newSetter(pdxPersistent, cacheFactorySpy))
					.when(cacheFactorySpy).setPdxPersistent(anyBoolean());

				doAnswer(newSetter(pdxReadSerialized, cacheFactorySpy))
					.when(cacheFactorySpy).setPdxReadSerialized(anyBoolean());

				doAnswer(newSetter(pdxSerializer, cacheFactorySpy))
					.when(cacheFactorySpy).setPdxSerializer(any(PdxSerializer.class));

				when(mockCache.getPdxDiskStore()).thenAnswer(newGetter(pdxDiskStoreName));
				when(mockCache.getPdxIgnoreUnreadFields()).thenAnswer(newGetter(pdxIgnoreUnreadFields));
				when(mockCache.getPdxPersistent()).thenAnswer(newGetter(pdxPersistent));
				when(mockCache.getPdxReadSerialized()).thenAnswer(newGetter(pdxReadSerialized));
				when(mockCache.getPdxSerializer()).thenAnswer(newGetter(pdxSerializer));

				return mockCache;
			});

		doReturn(rememberMockedGemFireCache(resolvedMockCache, useSingletonCache)).when(cacheFactorySpy).create();

		return cacheFactorySpy;
	}

	public static ClientCacheFactory spyOn(ClientCacheFactory clientCacheFactory) {
		return spyOn(clientCacheFactory, DEFAULT_USE_SINGLETON_CACHE);
	}

	public static ClientCacheFactory spyOn(ClientCacheFactory clientCacheFactory, boolean useSingletonCache) {

		ClientCacheFactory clientCacheFactorySpy = spy(clientCacheFactory);

		ClientCache resolvedMockedClientCache =
			MockGemFireObjectsSupport.<ClientCache>resolveMockedGemFireCache(useSingletonCache).orElseGet(() -> {

				ClientCache mockClientCache = mockClientCache();

				AtomicBoolean pdxIgnoreUnreadFields = new AtomicBoolean(false);
				AtomicBoolean pdxPersistent = new AtomicBoolean(false);
				AtomicBoolean pdxReadSerialized = new AtomicBoolean(false);

				AtomicReference<String> pdxDiskStoreName = new AtomicReference<>(null);
				AtomicReference<PdxSerializer> pdxSerializer = new AtomicReference<>(null);
				AtomicReference<Pool> defaultPool = new AtomicReference<>(null);

				doAnswer(newSetter(pdxDiskStoreName, clientCacheFactorySpy))
					.when(clientCacheFactorySpy).setPdxDiskStore(anyString());

				doAnswer(newSetter(pdxIgnoreUnreadFields, clientCacheFactorySpy))
					.when(clientCacheFactorySpy).setPdxIgnoreUnreadFields(anyBoolean());

				doAnswer(newSetter(pdxPersistent, clientCacheFactorySpy))
					.when(clientCacheFactorySpy).setPdxPersistent(anyBoolean());

				doAnswer(newSetter(pdxReadSerialized, clientCacheFactorySpy))
					.when(clientCacheFactorySpy).setPdxReadSerialized(anyBoolean());

				doAnswer(newSetter(pdxSerializer, clientCacheFactorySpy))
					.when(clientCacheFactorySpy).setPdxSerializer(any(PdxSerializer.class));

				PoolFactory mockPoolFactory = mockPoolFactory();

				doAnswer(invocation -> {
					mockPoolFactory.addLocator(invocation.getArgument(0), invocation.getArgument(1));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).addPoolLocator(anyString(), anyInt());

				doAnswer(invocation -> {
					mockPoolFactory.addServer(invocation.getArgument(0), invocation.getArgument(1));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).addPoolServer(anyString(), anyInt());

				doAnswer(invocation -> {
					mockPoolFactory.setFreeConnectionTimeout(invocation.getArgument(0));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).setPoolFreeConnectionTimeout(anyInt());

				doAnswer(invocation -> {
					mockPoolFactory.setIdleTimeout(invocation.getArgument(0));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).setPoolIdleTimeout(anyLong());

				doAnswer(invocation -> {
					mockPoolFactory.setLoadConditioningInterval(invocation.getArgument(0));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).setPoolLoadConditioningInterval(anyInt());

				doAnswer(invocation -> {
					mockPoolFactory.setMaxConnections(invocation.getArgument(0));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).setPoolMaxConnections(anyInt());

				doAnswer(invocation -> {
					mockPoolFactory.setMinConnections(invocation.getArgument(0));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).setPoolMinConnections(anyInt());

				doAnswer(invocation -> {
					mockPoolFactory.setMultiuserAuthentication(invocation.getArgument(0));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).setPoolMultiuserAuthentication(anyBoolean());

				doAnswer(invocation -> {
					mockPoolFactory.setPingInterval(invocation.getArgument(0));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).setPoolPingInterval(anyLong());

				doAnswer(invocation -> {
					mockPoolFactory.setPRSingleHopEnabled(invocation.getArgument(0));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).setPoolPRSingleHopEnabled(anyBoolean());

				doAnswer(invocation -> {
					mockPoolFactory.setReadTimeout(invocation.getArgument(0));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).setPoolReadTimeout(anyInt());

				doAnswer(invocation -> {
					mockPoolFactory.setRetryAttempts(invocation.getArgument(0));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).setPoolRetryAttempts(anyInt());

				doAnswer(invocation -> {
					mockPoolFactory.setServerGroup(invocation.getArgument(0));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).setPoolServerGroup(anyString());

				doAnswer(invocation -> {
					mockPoolFactory.setSocketBufferSize(invocation.getArgument(0));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).setPoolSocketBufferSize(anyInt());

				doAnswer(invocation -> {
					mockPoolFactory.setStatisticInterval(invocation.getArgument(0));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).setPoolStatisticInterval(anyInt());

				doAnswer(invocation -> {
					mockPoolFactory.setSubscriptionAckInterval(invocation.getArgument(0));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).setPoolSubscriptionAckInterval(anyInt());

				doAnswer(invocation -> {
					mockPoolFactory.setSubscriptionEnabled(invocation.getArgument(0));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).setPoolSubscriptionEnabled(anyBoolean());

				doAnswer(invocation -> {
					mockPoolFactory.setSubscriptionMessageTrackingTimeout(invocation.getArgument(0));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).setPoolSubscriptionMessageTrackingTimeout(anyInt());

				doAnswer(invocation -> {
					mockPoolFactory.setSubscriptionRedundancy(invocation.getArgument(0));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).setPoolSubscriptionRedundancy(anyInt());

				doAnswer(invocation -> {
					mockPoolFactory.setThreadLocalConnections(invocation.getArgument(0));
					return clientCacheFactorySpy;
				}).when(clientCacheFactorySpy).setPoolThreadLocalConnections(anyBoolean());

				when(mockClientCache.getCurrentServers()).thenAnswer(invocation ->
					Collections.unmodifiableSet(new HashSet<>(defaultPool.get().getServers())));

				when(mockClientCache.getDefaultPool()).thenAnswer(invocation -> {

					if (defaultPool.get() == null) {
						defaultPool.set(mockPoolFactory.create("DEFAULT"));
					}

					return defaultPool.get();
				});

				when(mockClientCache.getPdxDiskStore()).thenAnswer(newGetter(pdxDiskStoreName));
				when(mockClientCache.getPdxIgnoreUnreadFields()).thenAnswer(newGetter(pdxIgnoreUnreadFields));
				when(mockClientCache.getPdxPersistent()).thenAnswer(newGetter(pdxPersistent));
				when(mockClientCache.getPdxReadSerialized()).thenAnswer(newGetter(pdxReadSerialized));
				when(mockClientCache.getPdxSerializer()).thenAnswer(newGetter(pdxSerializer));

				return mockClientCache;
			});

		doReturn(rememberMockedGemFireCache(resolvedMockedClientCache, useSingletonCache))
			.when(clientCacheFactorySpy).create();

		return clientCacheFactorySpy;
	}
}
