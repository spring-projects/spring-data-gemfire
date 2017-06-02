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
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

import org.apache.commons.lang.StringUtils;
import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheFactory;
import org.apache.geode.cache.DiskStore;
import org.apache.geode.cache.DiskStoreFactory;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.RegionService;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientCacheFactory;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.client.PoolFactory;
import org.apache.geode.cache.control.ResourceManager;
import org.apache.geode.cache.server.CacheServer;
import org.apache.geode.cache.server.ClientSubscriptionConfig;
import org.apache.geode.distributed.DistributedSystem;
import org.apache.geode.pdx.PdxSerializer;
import org.springframework.data.gemfire.server.SubscriptionEvictionPolicy;
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

	private static final Map<String, DiskStore> diskStores = new ConcurrentHashMap<>();

	private static final Map<String, Region<Object, Object>> regions = new ConcurrentHashMap<>();

	private static final Map<String, RegionAttributes<Object, Object>> regionAttributes = new ConcurrentHashMap<>();

	/* (non-Javadoc) */
	private static boolean isRootRegion(Region<?, ?> region) {
		return isRootRegion(region.getFullPath());
	}

	/* (non-Javadoc) */
	private static boolean isRootRegion(String regionPath) {
		return (regionPath.lastIndexOf(Region.SEPARATOR) <= 0);
	}

	private static String toRegionPath(String regionPath) {
		return (StringUtils.startsWith(regionPath, Region.SEPARATOR) ? regionPath
			: String.format("%1$s%2$s", Region.SEPARATOR, regionPath));
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

		when(mockRegionService.rootRegions()).thenAnswer(invocation -> {

			Set<Region<Object, Object>> rootRegions = new HashSet<>();

			for (Region<Object, Object> region : regions.values()) {
				if (isRootRegion(region)) {
					rootRegions.add(region);
				}
			}

			return rootRegions;
		});

		return mockRegionService;
	}

	public static ClientCache mockClientCache() {

		ClientCache mockClientCache = mock(ClientCache.class);

		return mockCacheApi(mockClientCache);
	}

	public static GemFireCache mockGemFireCache() {

		GemFireCache mockGemFireCache = mock(GemFireCache.class);

		return mockCacheApi(mockGemFireCache);
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

		return mockCacheApi(mockCache);
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

	public static CacheFactory spyOn(CacheFactory cacheFactory) {

		CacheFactory cacheFactorySpy = spy(cacheFactory);

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

		doReturn(mockCache).when(cacheFactorySpy).create();

		when(mockCache.getPdxDiskStore()).thenAnswer(newGetter(pdxDiskStoreName));
		when(mockCache.getPdxIgnoreUnreadFields()).thenAnswer(newGetter(pdxIgnoreUnreadFields));
		when(mockCache.getPdxPersistent()).thenAnswer(newGetter(pdxPersistent));
		when(mockCache.getPdxReadSerialized()).thenAnswer(newGetter(pdxReadSerialized));
		when(mockCache.getPdxSerializer()).thenAnswer(newGetter(pdxSerializer));

		return cacheFactorySpy;
	}

	public static ClientCacheFactory spyOn(ClientCacheFactory clientCacheFactory) {

		ClientCacheFactory clientCacheFactorySpy = spy(clientCacheFactory);

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

		doReturn(mockClientCache).when(clientCacheFactorySpy).create();

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

		return clientCacheFactorySpy;
	}
}
