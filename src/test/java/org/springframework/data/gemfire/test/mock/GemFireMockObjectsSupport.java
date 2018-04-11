/*
 * Copyright 2017-2018 the original author or authors.
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

import static org.assertj.core.api.Assertions.assertThat;
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
import static org.springframework.data.gemfire.util.CollectionUtils.asSet;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeSet;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.NOT_SUPPORTED;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newUnsupportedOperationException;

import java.io.File;
import java.io.IOException;
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
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.geode.cache.AttributesMutator;
import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheFactory;
import org.apache.geode.cache.CacheListener;
import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.CacheWriter;
import org.apache.geode.cache.CustomExpiry;
import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.DiskStore;
import org.apache.geode.cache.DiskStoreFactory;
import org.apache.geode.cache.EvictionAttributes;
import org.apache.geode.cache.EvictionAttributesMutator;
import org.apache.geode.cache.ExpirationAction;
import org.apache.geode.cache.ExpirationAttributes;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.PartitionAttributes;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.RegionExistsException;
import org.apache.geode.cache.RegionFactory;
import org.apache.geode.cache.RegionService;
import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.Scope;
import org.apache.geode.cache.SubscriptionAttributes;
import org.apache.geode.cache.asyncqueue.AsyncEventListener;
import org.apache.geode.cache.asyncqueue.AsyncEventQueue;
import org.apache.geode.cache.asyncqueue.AsyncEventQueueFactory;
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
import org.apache.geode.cache.query.Index;
import org.apache.geode.cache.query.IndexStatistics;
import org.apache.geode.cache.query.Query;
import org.apache.geode.cache.query.QueryService;
import org.apache.geode.cache.query.QueryStatistics;
import org.apache.geode.cache.server.CacheServer;
import org.apache.geode.cache.server.ClientSubscriptionConfig;
import org.apache.geode.cache.wan.GatewayEventFilter;
import org.apache.geode.cache.wan.GatewayEventSubstitutionFilter;
import org.apache.geode.cache.wan.GatewayReceiver;
import org.apache.geode.cache.wan.GatewayReceiverFactory;
import org.apache.geode.cache.wan.GatewaySender;
import org.apache.geode.cache.wan.GatewaySenderFactory;
import org.apache.geode.cache.wan.GatewayTransportFilter;
import org.apache.geode.compression.Compressor;
import org.apache.geode.distributed.DistributedMember;
import org.apache.geode.distributed.DistributedSystem;
import org.apache.geode.internal.concurrent.ConcurrentHashSet;
import org.apache.geode.pdx.PdxSerializer;
import org.mockito.ArgumentMatchers;
import org.mockito.stubbing.Answer;
import org.springframework.data.gemfire.IndexType;
import org.springframework.data.gemfire.server.SubscriptionEvictionPolicy;
import org.springframework.data.gemfire.test.mock.support.MockObjectInvocationException;
import org.springframework.data.gemfire.test.support.FileSystemUtils;
import org.springframework.util.Assert;

/**
 * The {@link GemFireMockObjectsSupport} class is an abstract base class encapsulating factory methods for creating
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
@SuppressWarnings("all")
public abstract class GemFireMockObjectsSupport extends MockObjectsSupport {

	private static final boolean DEFAULT_USE_SINGLETON_CACHE = false;

	private static final AtomicReference<GemFireCache> singletonCache = new AtomicReference<>(null);

	private static final Map<String, DiskStore> diskStores = new ConcurrentHashMap<>();

	private static final Map<String, Region<Object, Object>> regions = new ConcurrentHashMap<>();

	private static final Map<String, RegionAttributes<Object, Object>> regionAttributes = new ConcurrentHashMap<>();

	private static final String FROM_KEYWORD = "FROM";
	private static final String WHERE_KEYWORD = "WHERE";

	private static final String REPEATING_REGION_SEPARATOR = Region.SEPARATOR + "{2,}";

	/**
	 * Destroys all mock object state.
	 */
	public static void destroy() {
		singletonCache.set(null);
		diskStores.clear();
		regions.clear();
		regionAttributes.clear();
	}

	/**
	 * Converts the given {@link ClientRegionShortcut} into a corresponding {@link DataPolicy}.
	 *
	 * @param clientRegionShortcut {@link ClientRegionShortcut} to convert.
	 * @return a {@link DataPolicy} from the {@link ClientRegionShortcut}.
	 * @see org.apache.geode.cache.client.ClientRegionShortcut
	 * @see org.apache.geode.cache.DataPolicy
	 */
	@SuppressWarnings("unchecked")
	private static DataPolicy convert(ClientRegionShortcut clientRegionShortcut) {

		return Optional.ofNullable(clientRegionShortcut).map(shortcut -> {

			switch(shortcut) {
				case CACHING_PROXY:
				case CACHING_PROXY_HEAP_LRU:
				case CACHING_PROXY_OVERFLOW:
				case LOCAL:
				case LOCAL_HEAP_LRU:
				case LOCAL_OVERFLOW:
					return DataPolicy.NORMAL;
				case LOCAL_PERSISTENT:
				case LOCAL_PERSISTENT_OVERFLOW:
					return DataPolicy.PERSISTENT_REPLICATE;
				case PROXY:
					return DataPolicy.EMPTY;
				default:
					return null;
			}

		}).orElse(DataPolicy.DEFAULT);
	}

	/**
	 * Converts the given {@link RegionShortcut} into a corresponding {@link DataPolicy}.
	 *
	 * @param regionShortcut {@link RegionShortcut} to convert.
	 * @return a {@link DataPolicy} from the {@link RegionShortcut}.
	 * @see org.apache.geode.cache.RegionShortcut
	 * @see org.apache.geode.cache.DataPolicy
	 */
	@SuppressWarnings("unchecked")
	private static DataPolicy convert(RegionShortcut regionShortcut) {

		return Optional.ofNullable(regionShortcut).map(shortcut -> {

			switch (shortcut) {
				case LOCAL:
				case LOCAL_HEAP_LRU:
				case LOCAL_OVERFLOW:
					return DataPolicy.NORMAL;
				case PARTITION:
				case PARTITION_HEAP_LRU:
				case PARTITION_OVERFLOW:
				case PARTITION_PROXY:
				case PARTITION_PROXY_REDUNDANT:
				case PARTITION_REDUNDANT:
				case PARTITION_REDUNDANT_HEAP_LRU:
				case PARTITION_REDUNDANT_OVERFLOW:
					return DataPolicy.PARTITION;
				case PARTITION_PERSISTENT:
				case PARTITION_PERSISTENT_OVERFLOW:
				case PARTITION_REDUNDANT_PERSISTENT:
				case PARTITION_REDUNDANT_PERSISTENT_OVERFLOW:
					return DataPolicy.PERSISTENT_PARTITION;
				case REPLICATE:
				case REPLICATE_HEAP_LRU:
				case REPLICATE_OVERFLOW:
					return DataPolicy.REPLICATE;
				case LOCAL_PERSISTENT:
				case LOCAL_PERSISTENT_OVERFLOW:
				case REPLICATE_PERSISTENT:
				case REPLICATE_PERSISTENT_OVERFLOW:
					return DataPolicy.PERSISTENT_REPLICATE;
				case REPLICATE_PROXY:
					return DataPolicy.EMPTY;
				default:
					return null;
			}

		}).orElse(DataPolicy.DEFAULT);
	}

	/**
	 * Executes the given {@link IoExceptionThrowingOperation}, handling any {@link IOException IOExceptions} thrown
	 * during normal IO processing.
	 *
	 * @param operation {@link IoExceptionThrowingOperation} to execute.
	 * @return a boolean indicating whether the IO operation was successful, or {@literal false} if the IO operation
	 * threw an {@link IOException}.
	 * @see IOException
	 */
	private static boolean doSafeIo(IoExceptionThrowingOperation operation) {

		try {
			operation.doIo();
			return true;
		}
		catch (IOException cause) {
			return false;
		}
	}

	/**
	 * Determines whether the given {@link Region} is a root {@link Region}.
	 *
	 * @param region {@link Region} to evaluate.
	 * @return a boolean value indicating whether the {@link Region} is a root {@link Region}.
	 * @see org.apache.geode.cache.Region
	 * @see #isRootRegion(String)
	 */
	private static boolean isRootRegion(Region<?, ?> region) {
		return isRootRegion(region.getFullPath());
	}

	/**
	 * Determines whether the {@link Region} identified by the given {@link String path} is a root {@link Region}.
	 *
	 * @param regionPath {@link String path} identifying the {@link Region} to evaluate.
	 * @return a boolean value indicating whether the {@link Region} identified by the given {@link String path}
	 * is a root {@link Region}.
	 */
	private static boolean isRootRegion(String regionPath) {
		return regionPath.lastIndexOf(Region.SEPARATOR) <= 0;
	}

	/**
	 * Normalizes the given {@link Region#getFullPath() Regon path} by removing all duplicate, repeating
	 * {@link Region#SEPARATOR} characters between path segments as well as removing the trailing
	 * {@link Region#SEPARATOR}.
	 *
	 * @param regionPath {@link Region#getFullPath()} to normalize.
	 * @return a normalized version of the given {@link Region#getFullPath()}.
	 */
	private static String normalizeRegionPath(String regionPath) {

		regionPath = regionPath.replaceAll(REPEATING_REGION_SEPARATOR, Region.SEPARATOR);

		regionPath = regionPath.endsWith(Region.SEPARATOR)
			? regionPath.substring(0, regionPath.length() - 1)
			: regionPath;

		return regionPath;
	}

	/**
	 * Remembers the given mock {@link GemFireCache} object, which may be a {@link ClientCache} or a peer {@link Cache}.
	 *
	 * @param <T> {@link Class sub-type} of the {@link GemFireCache} instance.
	 * @param mockedGemFireCache {@link GemFireCache} to remember.
	 * @param useSingletonCache boolean value indicating whether the {@link GemFireCache} is a Singleton.
	 * @return the given {@link GemFireCache}.
	 * @throws IllegalArgumentException if {@link GemFireCache} is {@literal null}.
	 * @see org.apache.geode.cache.GemFireCache
	 */
	private static <T extends GemFireCache> T rememberMockedGemFireCache(T mockedGemFireCache,
		boolean useSingletonCache) {

		return Optional.ofNullable(mockedGemFireCache)
			.map(it -> {

				if (useSingletonCache) {
					singletonCache.compareAndSet(null, it);
				}

				return it;
			})
			.orElseThrow(() -> newIllegalArgumentException("GemFireCache is required"));
	}

	/**
	 * Remembers the given mock {@link Region}.
	 *
	 * @param <K> {@link Class type} of the {@link Region} key.
	 * @param <V> {@link Class type} of the {@link Region} value.
	 * @param mockRegion {@link Region} to remember.
	 * @throws IllegalArgumentException if the given {@link Region} is {@literal null}.
	 * @throws RegionExistsException if the given {@link Region} already exists.
	 * @return the given {@link Region}.
	 * @see org.apache.geode.cache.Region
	 */
	@SuppressWarnings("unchecked")
	private static <K, V> Region<K, V> rememberMockedRegion(Region<K, V> mockRegion) {

		String mockRegionPath = Optional.ofNullable(mockRegion).map(Region::getFullPath)
			.orElseThrow(() -> newIllegalArgumentException("Region is required"));

		if (regions.putIfAbsent(mockRegionPath, (Region) mockRegion) != null) {
			throw new RegionExistsException(mockRegion);
		}

		assertThat(regions).containsValue((Region) mockRegion);

		return mockRegion;
	}

	/**
	 * Resolves the single, remembered {@link GemFireCache} if using GemFire in Singleton-mode.
	 *
	 * @param <T> {@link Class sub-type} of the {@link GemFireCache} instance.
	 * @param useSingletonCache boolean value indicating if mock infrastructure is using GemFire Singletons.
	 * @return an {@link Optional}, single remembered instance of the {@link GemFireCache}.
	 * @see org.apache.geode.cache.GemFireCache
	 */
	@SuppressWarnings("unchecked")
	private static <T extends GemFireCache> Optional<T> resolveMockedGemFireCache(boolean useSingletonCache) {
		return Optional.ofNullable((T) singletonCache.get()).filter(it -> useSingletonCache);
	}

	/**
	 * Resolves the {@link RegionAttributes} identified by the given {@link String id}.
	 *
	 * @param <K> {@link Class type} of the {@link Region} key.
	 * @param <V> {@link Class type} of the {@link Region} value.
	 * @param regionAttributesId {@link String id} identifying the {@link RegionAttributes} to resolve.
	 * @return the resolved {@link RegionAttributes} identified by the given {@link String id}.
	 * @throws IllegalStateException if {@link RegionAttributes} could not be resolved from the given {@link String id}.
	 * @see org.apache.geode.cache.RegionAttributes
	 */
	@SuppressWarnings("unchecked")
	private static <K, V> RegionAttributes<K, V> resolveRegionAttributes(String regionAttributesId) {

		return (RegionAttributes<K, V>) Optional.ofNullable(regionAttributes.get(regionAttributesId)).orElseThrow(() ->
			newIllegalStateException("RegionAttributes with ID [%s] cannot be found", regionAttributesId));
	}

	/**
	 * Converts the given {@link String Region name} into a proper {@link Region#getName() Region name}.
	 *
	 * @param regionName {@link String Region name} to evaluate.
	 * @return a proper {@link Region#getName() Region name} from the given {@link String Region name}.
	 * @throws IllegalArgumentException if {@link String Region name} is {@literal null}
	 * or {@link String#isEmpty() empty}.
	 * @see String
	 */
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

	/**
	 * Converts the given {@link String Region path} into a proper {@link Region#getFullPath() Region path}.
	 *
	 * @param regionPath {@link String Region path} to evaluate.
	 * @return a proper {@link Region#getFullPath() Region path} from the given {@link String Region path}.
	 * @throws IllegalArgumentException if {@link String Region path} is {@literal null}
	 * or {@link String#isEmpty() empty}.
	 * @see String
	 */
	private static String toRegionPath(String regionPath) {

		return Optional.ofNullable(regionPath)
			.map(String::trim)
			.map(it -> it.startsWith(Region.SEPARATOR) ? it : String.format("%1$s%2$s", Region.SEPARATOR, it))
			.map(GemFireMockObjectsSupport::normalizeRegionPath)
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

		doThrow(newUnsupportedOperationException(NOT_SUPPORTED))
			.when(mockGemFireCache).loadCacheXml(any(InputStream.class));

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

			String resolvedRegionPath = Optional.ofNullable(regionPath)
				.map(String::trim)
				.filter(it -> !it.isEmpty())
				.map(GemFireMockObjectsSupport::toRegionPath)
				.orElseThrow(() -> newIllegalArgumentException("Region path [%s] is not valid", regionPath));

			return regions.get(resolvedRegionPath);
		});

		when(mockRegionService.createPdxEnum(anyString(), anyString(), anyInt()))
			.thenThrow(newUnsupportedOperationException(NOT_SUPPORTED));

		when(mockRegionService.createPdxInstanceFactory(anyString()))
			.thenThrow(newUnsupportedOperationException(NOT_SUPPORTED));

		when(mockRegionService.rootRegions()).thenAnswer(invocation ->
			regions.values().stream().filter(GemFireMockObjectsSupport::isRootRegion).collect(Collectors.toSet()));

		return mockRegionService;
	}

	public static ClientCache mockClientCache() {

		ClientCache mockClientCache = mock(ClientCache.class);

		doAnswer(newVoidAnswer(invocation -> mockClientCache.close())).when(mockClientCache).close(anyBoolean());

		when(mockClientCache.createClientRegionFactory(any(ClientRegionShortcut.class))).thenAnswer(invocation ->
			mockClientRegionFactory(mockClientCache, invocation.<ClientRegionShortcut>getArgument(0)));

		when(mockClientCache.createClientRegionFactory(anyString())).thenAnswer(invocation ->
			mockClientRegionFactory(mockClientCache, invocation.<String>getArgument(0)));

		return mockQueryService(mockCacheApi(mockClientCache));
	}

	public static GemFireCache mockGemFireCache() {

		GemFireCache mockGemFireCache = mock(GemFireCache.class);

		return mockQueryService(mockCacheApi(mockGemFireCache));
	}

	@SuppressWarnings("unchecked")
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

		when(mockCache.createRegionFactory()).thenAnswer(invocation -> mockRegionFactory(mockCache));

		when(mockCache.createRegionFactory(any(RegionAttributes.class))).thenAnswer(invocation ->
			mockRegionFactory(mockCache, invocation.<RegionAttributes<?, ?>>getArgument(0)));

		when(mockCache.createRegionFactory(any(RegionShortcut.class))).thenAnswer(invocation ->
			mockRegionFactory(mockCache, invocation.<RegionShortcut>getArgument(0)));

		when(mockCache.createRegionFactory(anyString())).thenAnswer(invocation ->
			mockRegionFactory(mockCache, invocation.<String>getArgument(0)));

		return mockQueryService(
			mockGatewaySenderFactory(
				mockGatewayReceiverFactory(
					mockAsyncEventQueueFactory(
						mockCacheApi(mockCache)))));
	}

	public static Cache mockAsyncEventQueueFactory(Cache mockCache) {

		when(mockCache.createAsyncEventQueueFactory()).thenAnswer(invocation -> mockAsyncEventQueueFactory());

		return mockCache;
	}

	public static AsyncEventQueueFactory mockAsyncEventQueueFactory() {

		AsyncEventQueueFactory mockAsyncEventQueueFactory = mock(AsyncEventQueueFactory.class);

		AtomicBoolean batchConflationEnabled = new AtomicBoolean(false);
		AtomicBoolean diskSynchronous = new AtomicBoolean(true);
		AtomicBoolean forwardExpirationDestroy = new AtomicBoolean(false);
		AtomicBoolean parallel = new AtomicBoolean(false);
		AtomicBoolean persistent = new AtomicBoolean(false);

		AtomicInteger batchSize = new AtomicInteger(100);
		AtomicInteger batchTimeInterval = new AtomicInteger(5);
		AtomicInteger dispatcherThreads = new AtomicInteger(5);
		AtomicInteger maximumQueueMemory = new AtomicInteger(100);

		AtomicReference<String> diskStoreName = new AtomicReference<>(null);
		AtomicReference<GatewayEventSubstitutionFilter> gatewayEventSubstitutionFilter =
			new AtomicReference<>(null);
		AtomicReference<GatewaySender.OrderPolicy> orderPolicy = new AtomicReference<>(GatewaySender.OrderPolicy.KEY);

		CopyOnWriteArrayList<GatewayEventFilter> gatewayEventFilters = new CopyOnWriteArrayList<>();

		when(mockAsyncEventQueueFactory.setBatchConflationEnabled(anyBoolean()))
			.thenAnswer(newSetter(batchConflationEnabled, mockAsyncEventQueueFactory));

		when(mockAsyncEventQueueFactory.setBatchSize(anyInt()))
			.thenAnswer(newSetter(batchSize, mockAsyncEventQueueFactory));

		when(mockAsyncEventQueueFactory.setBatchTimeInterval(anyInt()))
			.thenAnswer(newSetter(batchTimeInterval, mockAsyncEventQueueFactory));

		when(mockAsyncEventQueueFactory.setDiskStoreName(anyString()))
			.thenAnswer(newSetter(diskStoreName, mockAsyncEventQueueFactory));

		when(mockAsyncEventQueueFactory.setDiskSynchronous(anyBoolean()))
			.thenAnswer(newSetter(diskSynchronous, mockAsyncEventQueueFactory));

		when(mockAsyncEventQueueFactory.setDispatcherThreads(anyInt()))
			.thenAnswer(newSetter(dispatcherThreads, mockAsyncEventQueueFactory));

		when(mockAsyncEventQueueFactory.setForwardExpirationDestroy(anyBoolean()))
			.thenAnswer(newSetter(forwardExpirationDestroy, mockAsyncEventQueueFactory));

		when(mockAsyncEventQueueFactory.setGatewayEventSubstitutionListener(any(GatewayEventSubstitutionFilter.class)))
			.thenAnswer(newSetter(gatewayEventSubstitutionFilter, mockAsyncEventQueueFactory));

		when(mockAsyncEventQueueFactory.setMaximumQueueMemory(anyInt()))
			.thenAnswer(newSetter(maximumQueueMemory, mockAsyncEventQueueFactory));

		when(mockAsyncEventQueueFactory.setOrderPolicy(any(GatewaySender.OrderPolicy.class)))
			.thenAnswer(newSetter(orderPolicy, mockAsyncEventQueueFactory));

		when(mockAsyncEventQueueFactory.setParallel(anyBoolean()))
			.thenAnswer(newSetter(parallel, mockAsyncEventQueueFactory));

		when(mockAsyncEventQueueFactory.setPersistent(anyBoolean()))
			.thenAnswer(newSetter(persistent, mockAsyncEventQueueFactory));

		when(mockAsyncEventQueueFactory.addGatewayEventFilter(any(GatewayEventFilter.class))).thenAnswer(invocation -> {

			gatewayEventFilters.add(invocation.getArgument(0));

			return mockAsyncEventQueueFactory;
		});

		when(mockAsyncEventQueueFactory.removeGatewayEventFilter(any(GatewayEventFilter.class))).thenAnswer(invocation -> {

			gatewayEventFilters.remove(invocation.<GatewayEventFilter>getArgument(0));

			return mockAsyncEventQueueFactory;
		});

		when(mockAsyncEventQueueFactory.create(anyString(), any(AsyncEventListener.class))).thenAnswer(invocation -> {

			String asyncEventQueueId = invocation.getArgument(0);

			AsyncEventListener listener = invocation.getArgument(1);

			AsyncEventQueue mockAsyncEventQueue = mock(AsyncEventQueue.class);

			when(mockAsyncEventQueue.isBatchConflationEnabled()).thenAnswer(newGetter(batchConflationEnabled));
			when(mockAsyncEventQueue.isDiskSynchronous()).thenAnswer(newGetter(diskSynchronous));
			when(mockAsyncEventQueue.isForwardExpirationDestroy()).thenAnswer(newGetter(forwardExpirationDestroy));
			when(mockAsyncEventQueue.isParallel()).thenAnswer(newGetter(parallel));
			when(mockAsyncEventQueue.isPersistent()).thenAnswer(newGetter(persistent));
			when(mockAsyncEventQueue.isPrimary()).thenReturn(false);

			when(mockAsyncEventQueue.getAsyncEventListener()).thenReturn(listener);
			when(mockAsyncEventQueue.getBatchSize()).thenAnswer(newGetter(batchSize));
			when(mockAsyncEventQueue.getBatchTimeInterval()).thenAnswer(newGetter(batchTimeInterval));
			when(mockAsyncEventQueue.getDiskStoreName()).thenAnswer(newGetter(diskStoreName));
			when(mockAsyncEventQueue.getDispatcherThreads()).thenAnswer(newGetter(dispatcherThreads));
			when(mockAsyncEventQueue.getGatewayEventFilters()).thenReturn(gatewayEventFilters);
			when(mockAsyncEventQueue.getGatewayEventSubstitutionFilter()).thenAnswer(newGetter(gatewayEventSubstitutionFilter));
			when(mockAsyncEventQueue.getId()).thenReturn(asyncEventQueueId);
			when(mockAsyncEventQueue.getMaximumQueueMemory()).thenAnswer(newGetter(maximumQueueMemory));
			when(mockAsyncEventQueue.getOrderPolicy()).thenAnswer(newGetter(orderPolicy));

			when(mockAsyncEventQueue.size()).thenReturn(0);

			return mockAsyncEventQueue;
		});

		return mockAsyncEventQueueFactory;
	}

	public static CacheServer mockCacheServer() {

		CacheServer mockCacheServer = mock(CacheServer.class);

		AtomicBoolean running = new AtomicBoolean(false);
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

		when(mockCacheServer.isRunning()).thenAnswer(newGetter(running));
		when(mockCacheServer.getAllClientSessions()).thenReturn(Collections.emptySet());
		when(mockCacheServer.getBindAddress()).thenAnswer(newGetter(bindAddress));
		when(mockCacheServer.getClientSession(any(DistributedMember.class)))
			.thenThrow(newUnsupportedOperationException(NOT_SUPPORTED));
		when(mockCacheServer.getClientSession(anyString())).thenThrow(newUnsupportedOperationException(NOT_SUPPORTED));
		when(mockCacheServer.getHostnameForClients()).thenAnswer(newGetter(hostnameForClients));
		when(mockCacheServer.getInterestRegistrationListeners()).thenReturn(Collections.emptySet());
		when(mockCacheServer.getLoadPollInterval()).thenAnswer(newGetter(loadPollInterval));
		when(mockCacheServer.getLoadProbe()).thenThrow(newUnsupportedOperationException(NOT_SUPPORTED));
		when(mockCacheServer.getMaxConnections()).thenAnswer(newGetter(maxConnections));
		when(mockCacheServer.getMaximumMessageCount()).thenAnswer(newGetter(maxMessageCount));
		when(mockCacheServer.getMaximumTimeBetweenPings()).thenAnswer(newGetter(maxTimeBetweenPings));
		when(mockCacheServer.getMaxThreads()).thenAnswer(newGetter(maxThreads));
		when(mockCacheServer.getMessageTimeToLive()).thenAnswer(newGetter(messageTimeToLive));
		when(mockCacheServer.getPort()).thenAnswer(newGetter(port));
		when(mockCacheServer.getSocketBufferSize()).thenAnswer(newGetter(socketBufferSize));
		when(mockCacheServer.getTcpNoDelay()).thenAnswer(newGetter(tcpNoDelay));

		ClientSubscriptionConfig mockClientSubscriptionConfig = mockClientSubscriptionConfig();

		when(mockCacheServer.getClientSubscriptionConfig()).thenReturn(mockClientSubscriptionConfig);

		doSafeIo(() -> doAnswer(newSetter(running, true, null)).when(mockCacheServer).start());
		doAnswer(newSetter(running, false, null)).when(mockCacheServer).stop();

		return mockCacheServer;
	}

	public static <K, V> ClientRegionFactory<K, V> mockClientRegionFactory(ClientCache mockClientCache,
		ClientRegionShortcut clientRegionShortcut) {

		return mockClientRegionFactory(mockClientCache, clientRegionShortcut, null);
	}

	public static <K, V> ClientRegionFactory<K, V> mockClientRegionFactory(ClientCache mockClientCache,
		String regionAttributesId) {

		return mockClientRegionFactory(mockClientCache, null,
			resolveRegionAttributes(regionAttributesId));
	}

	@SuppressWarnings("unchecked")
	public static <K, V> ClientRegionFactory<K, V> mockClientRegionFactory(ClientCache mockClientCache,
		ClientRegionShortcut clientRegionShortcut, RegionAttributes<K, V> regionAttributes) {

		ClientRegionFactory<K, V> mockClientRegionFactory =
			mock(ClientRegionFactory.class, mockObjectIdentifier("MockClientRegionFactory"));

		ExpirationAttributes DEFAULT_EXPIRATION_ATTRIBUTES =
			new ExpirationAttributes(0, ExpirationAction.INVALIDATE);

		Optional<RegionAttributes<K, V>> optionalRegionAttributes = Optional.ofNullable(regionAttributes);

		AtomicBoolean cloningEnabled = new AtomicBoolean(optionalRegionAttributes
			.map(RegionAttributes::getCloningEnabled).orElse(false));

		AtomicBoolean concurrencyChecksEnabled = new AtomicBoolean(optionalRegionAttributes
			.map(RegionAttributes::getConcurrencyChecksEnabled).orElse(false));

		AtomicBoolean diskSynchronous = new AtomicBoolean(optionalRegionAttributes
			.map(RegionAttributes::isDiskSynchronous).orElse(true));

		AtomicBoolean statisticsEnabled = new AtomicBoolean(optionalRegionAttributes
			.map(RegionAttributes::getStatisticsEnabled).orElse(false));

		AtomicInteger concurrencyLevel = new AtomicInteger(optionalRegionAttributes
			.map(RegionAttributes::getConcurrencyLevel).orElse(16));

		AtomicInteger initialCapacity = new AtomicInteger(optionalRegionAttributes
			.map(RegionAttributes::getInitialCapacity).orElse(16));

		AtomicReference<Compressor> compressor = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getCompressor).orElse(null));

		AtomicReference<CustomExpiry<K, V>> customEntryIdleTimeout = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getCustomEntryIdleTimeout).orElse(null));

		AtomicReference<CustomExpiry<K, V>> customEntryTimeToLive = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getCustomEntryTimeToLive).orElse(null));

		AtomicReference<DataPolicy> dataPolicy = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getDataPolicy).orElseGet(() -> convert(clientRegionShortcut)));

		AtomicReference<String> diskStoreName = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getDiskStoreName).orElse(null));

		AtomicReference<ExpirationAttributes> entryIdleTimeout = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getEntryIdleTimeout).orElse(DEFAULT_EXPIRATION_ATTRIBUTES));

		AtomicReference<ExpirationAttributes> entryTimeToLive = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getEntryTimeToLive).orElse(DEFAULT_EXPIRATION_ATTRIBUTES));

		AtomicReference<EvictionAttributes> evictionAttributes = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getEvictionAttributes).orElseGet(EvictionAttributes::createLRUEntryAttributes));

		AtomicReference<Class<K>> keyConstraint = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getKeyConstraint).orElse(null));

		AtomicReference<Float> loadFactor = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getLoadFactor).orElse(0.75f));

		AtomicReference<String> poolName = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getPoolName).orElse(null));

		AtomicReference<ExpirationAttributes> regionIdleTimeout = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getRegionIdleTimeout).orElse(DEFAULT_EXPIRATION_ATTRIBUTES));

		AtomicReference<ExpirationAttributes> regionTimeToLive = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getRegionTimeToLive).orElse(DEFAULT_EXPIRATION_ATTRIBUTES));

		AtomicReference<Class<V>> valueConstraint = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getValueConstraint).orElse(null));

		List<CacheListener> cacheListeners = new ArrayList<>(Arrays.asList(nullSafeArray(optionalRegionAttributes
			.map(RegionAttributes::getCacheListeners).orElse(null), CacheListener.class)));

		when(mockClientRegionFactory.addCacheListener(any(CacheListener.class)))
			.thenAnswer(newAdder(cacheListeners, mockClientRegionFactory));

		when(mockClientRegionFactory.initCacheListeners(any(CacheListener[].class))).thenAnswer(invocation -> {
			cacheListeners.clear();
			Collections.addAll(cacheListeners, invocation.getArgument(0));
			return mockClientRegionFactory;
		});

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

		when(mockRegionAttributes.getCacheListeners())
			.thenAnswer(newGetter(() -> cacheListeners.toArray(new CacheListener[cacheListeners.size()])));

		when(mockRegionAttributes.getCloningEnabled()).thenAnswer(newGetter(cloningEnabled));
		when(mockRegionAttributes.getCompressor()).thenAnswer(newGetter(compressor));
		when(mockRegionAttributes.getConcurrencyChecksEnabled()).thenAnswer(newGetter(concurrencyChecksEnabled));
		when(mockRegionAttributes.getConcurrencyLevel()).thenAnswer(newGetter(concurrencyLevel));
		when(mockRegionAttributes.getCustomEntryIdleTimeout()).thenAnswer(newGetter(customEntryIdleTimeout));
		when(mockRegionAttributes.getCustomEntryTimeToLive()).thenAnswer(newGetter(customEntryTimeToLive));
		when(mockRegionAttributes.getDataPolicy()).thenAnswer(newGetter(dataPolicy));
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

	public static Cache mockGatewayReceiverFactory(Cache mockCache) {

		when(mockCache.createGatewayReceiverFactory()).thenAnswer(invocation -> mockGatewayReceiverFactory());

		return mockCache;
	}

	public static GatewayReceiverFactory mockGatewayReceiverFactory() {

		GatewayReceiverFactory mockGatewayReceiverFactory = mock(GatewayReceiverFactory.class);

		AtomicBoolean manualStart = new AtomicBoolean(GatewayReceiver.DEFAULT_MANUAL_START);

		AtomicInteger endPort = new AtomicInteger(GatewayReceiver.DEFAULT_END_PORT);
		AtomicInteger maximumTimeBetweenPings = new AtomicInteger(GatewayReceiver.DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS);
		AtomicInteger socketBufferSize = new AtomicInteger(GatewayReceiver.DEFAULT_SOCKET_BUFFER_SIZE);
		AtomicInteger startPort = new AtomicInteger(GatewayReceiver.DEFAULT_START_PORT);

		AtomicReference<String> bindAddress = new AtomicReference<>(GatewayReceiver.DEFAULT_BIND_ADDRESS);
		AtomicReference<String> hostnameForSenders = new AtomicReference<>(GatewayReceiver.DEFAULT_HOSTNAME_FOR_SENDERS);

		CopyOnWriteArrayList<GatewayTransportFilter> gatewayTransportFilters = new CopyOnWriteArrayList<>();

		when(mockGatewayReceiverFactory.setBindAddress(anyString()))
			.thenAnswer(newSetter(bindAddress, mockGatewayReceiverFactory));

		when(mockGatewayReceiverFactory.setEndPort(anyInt()))
			.thenAnswer(newSetter(endPort, mockGatewayReceiverFactory));

		when(mockGatewayReceiverFactory.setHostnameForSenders(anyString()))
			.thenAnswer(newSetter(hostnameForSenders, mockGatewayReceiverFactory));

		when(mockGatewayReceiverFactory.setManualStart(anyBoolean()))
			.thenAnswer(newSetter(manualStart, mockGatewayReceiverFactory));

		when(mockGatewayReceiverFactory.setMaximumTimeBetweenPings(anyInt()))
			.thenAnswer(newSetter(maximumTimeBetweenPings, mockGatewayReceiverFactory));

		when(mockGatewayReceiverFactory.setSocketBufferSize(anyInt()))
			.thenAnswer(newSetter(socketBufferSize, mockGatewayReceiverFactory));

		when(mockGatewayReceiverFactory.setStartPort(anyInt()))
			.thenAnswer(newSetter(startPort, mockGatewayReceiverFactory));

		when(mockGatewayReceiverFactory.addGatewayTransportFilter(any(GatewayTransportFilter.class))).thenAnswer(invocation -> {

			gatewayTransportFilters.add(invocation.getArgument(0));

			return mockGatewayReceiverFactory;
		});

		when(mockGatewayReceiverFactory.removeGatewayTransportFilter(any(GatewayTransportFilter.class))).thenAnswer(invocation -> {

			gatewayTransportFilters.remove(invocation.<GatewayTransportFilter>getArgument(0));

			return mockGatewayReceiverFactory;
		});

		when(mockGatewayReceiverFactory.create()).thenAnswer(invocation -> {

			AtomicBoolean running = new AtomicBoolean(false);

			GatewayReceiver mockGatewayReceiver = mock(GatewayReceiver.class);

			when(mockGatewayReceiver.isManualStart()).thenAnswer(newGetter(manualStart));
			when(mockGatewayReceiver.isRunning()).thenAnswer(newGetter(running));

			when(mockGatewayReceiver.getBindAddress()).thenAnswer(newGetter(bindAddress));
			when(mockGatewayReceiver.getEndPort()).thenAnswer(newGetter(endPort));
			when(mockGatewayReceiver.getGatewayTransportFilters()).thenReturn(gatewayTransportFilters);
			when(mockGatewayReceiver.getHost()).thenAnswer(newGetter(hostnameForSenders));
			//when(mockGatewayReceiver.getHostnameForSenders()).thenAnswer(newGetter(hostnameForSenders));
			when(mockGatewayReceiver.getMaximumTimeBetweenPings()).thenAnswer(newGetter(maximumTimeBetweenPings));
			when(mockGatewayReceiver.getPort()).thenReturn(0);
			when(mockGatewayReceiver.getSocketBufferSize()).thenAnswer(newGetter(socketBufferSize));
			when(mockGatewayReceiver.getStartPort()).thenAnswer(newGetter(startPort));

			doAnswer(newSetter(running, true, null)).when(mockGatewayReceiver).start();
			doAnswer(newSetter(running, false, null)).when(mockGatewayReceiver).stop();

			return mockGatewayReceiver;
		});

		return mockGatewayReceiverFactory;
	}

	public static Cache mockGatewaySenderFactory(Cache mockCache) {

		when(mockCache.createGatewaySenderFactory()).thenAnswer(invocation -> mockGatewaySenderFactory());

		return mockCache;
	}

	public static GatewaySenderFactory mockGatewaySenderFactory() {

		GatewaySenderFactory mockGatewaySenderFactory = mock(GatewaySenderFactory.class);

		AtomicBoolean batchConflationEnabled = new AtomicBoolean(GatewaySender.DEFAULT_BATCH_CONFLATION);
		AtomicBoolean diskSynchronous = new AtomicBoolean(GatewaySender.DEFAULT_DISK_SYNCHRONOUS);
		AtomicBoolean parallel = new AtomicBoolean(GatewaySender.DEFAULT_IS_PARALLEL);
		AtomicBoolean persistenceEnabled = new AtomicBoolean(GatewaySender.DEFAULT_PERSISTENCE_ENABLED);

		AtomicInteger alertThreshold = new AtomicInteger(GatewaySender.DEFAULT_ALERT_THRESHOLD);
		AtomicInteger batchSize = new AtomicInteger(GatewaySender.DEFAULT_BATCH_SIZE);
		AtomicInteger batchTimeInterval = new AtomicInteger(GatewaySender.DEFAULT_BATCH_TIME_INTERVAL);
		AtomicInteger dispatcherThreads = new AtomicInteger(GatewaySender.DEFAULT_DISPATCHER_THREADS);
		AtomicInteger maximumQueueMemory = new AtomicInteger(GatewaySender.DEFAULT_MAXIMUM_QUEUE_MEMORY);
		AtomicInteger parallelFactorForReplicatedRegion =
			new AtomicInteger(GatewaySender.DEFAULT_PARALLELISM_REPLICATED_REGION);
		AtomicInteger socketBufferSize = new AtomicInteger(GatewaySender.DEFAULT_SOCKET_BUFFER_SIZE);
		AtomicInteger socketReadTimeout = new AtomicInteger(GatewaySender.DEFAULT_SOCKET_READ_TIMEOUT);

		AtomicReference<String> diskStoreName = new AtomicReference<>(null);
		AtomicReference<GatewayEventSubstitutionFilter> gatewayEventSubstitutionFilter =
			new AtomicReference<>(null);
		AtomicReference<GatewaySender.OrderPolicy> orderPolicy = new AtomicReference<>(GatewaySender.DEFAULT_ORDER_POLICY);

		CopyOnWriteArrayList<GatewayEventFilter> gatewayEventFilters = new CopyOnWriteArrayList<>();
		CopyOnWriteArrayList<GatewayTransportFilter> gatewayTransportFilters = new CopyOnWriteArrayList<>();

		when(mockGatewaySenderFactory.setAlertThreshold(anyInt()))
			.thenAnswer(newSetter(alertThreshold, mockGatewaySenderFactory));

		when(mockGatewaySenderFactory.setBatchConflationEnabled(anyBoolean()))
			.thenAnswer(newSetter(batchConflationEnabled, mockGatewaySenderFactory));

		when(mockGatewaySenderFactory.setBatchSize(anyInt())).thenAnswer(newSetter(batchSize, mockGatewaySenderFactory));

		when(mockGatewaySenderFactory.setBatchTimeInterval(anyInt()))
			.thenAnswer(newSetter(batchTimeInterval, mockGatewaySenderFactory));

		when(mockGatewaySenderFactory.setDiskStoreName(anyString()))
			.thenAnswer(newSetter(diskStoreName, mockGatewaySenderFactory));

		when(mockGatewaySenderFactory.setDiskSynchronous(anyBoolean()))
			.thenAnswer(newSetter(diskSynchronous, mockGatewaySenderFactory));

		when(mockGatewaySenderFactory.setDispatcherThreads(anyInt()))
			.thenAnswer(newSetter(dispatcherThreads, mockGatewaySenderFactory));

		when(mockGatewaySenderFactory.setGatewayEventSubstitutionFilter(any(GatewayEventSubstitutionFilter.class)))
			.thenAnswer(newSetter(gatewayEventSubstitutionFilter, mockGatewaySenderFactory));

		when(mockGatewaySenderFactory.setMaximumQueueMemory(anyInt()))
			.thenAnswer(newSetter(maximumQueueMemory, mockGatewaySenderFactory));

		when(mockGatewaySenderFactory.setOrderPolicy(any(GatewaySender.OrderPolicy.class)))
			.thenAnswer(newSetter(orderPolicy, mockGatewaySenderFactory));

		when(mockGatewaySenderFactory.setParallel(anyBoolean())).thenAnswer(newSetter(parallel, mockGatewaySenderFactory));

		when(mockGatewaySenderFactory.setParallelFactorForReplicatedRegion(anyInt()))
			.thenAnswer(newSetter(parallelFactorForReplicatedRegion, mockGatewaySenderFactory));

		when(mockGatewaySenderFactory.setPersistenceEnabled(anyBoolean()))
			.thenAnswer(newSetter(persistenceEnabled, mockGatewaySenderFactory));

		when(mockGatewaySenderFactory.setSocketBufferSize(anyInt()))
			.thenAnswer(newSetter(socketBufferSize, mockGatewaySenderFactory));

		when(mockGatewaySenderFactory.setSocketReadTimeout(anyInt()))
			.thenAnswer(newSetter(socketReadTimeout, mockGatewaySenderFactory));

		when(mockGatewaySenderFactory.addGatewayEventFilter(any(GatewayEventFilter.class))).thenAnswer(invocation -> {

			gatewayEventFilters.add(invocation.getArgument(0));

			return mockGatewaySenderFactory;
		});

		when(mockGatewaySenderFactory.addGatewayTransportFilter(any(GatewayTransportFilter.class))).thenAnswer(invocation -> {

			gatewayTransportFilters.add(invocation.getArgument(0));

			return mockGatewaySenderFactory;
		});

		when(mockGatewaySenderFactory.removeGatewayEventFilter(any(GatewayEventFilter.class))).thenAnswer(invocation -> {

			gatewayEventFilters.remove(invocation.<GatewayEventFilter>getArgument(0));

			return mockGatewaySenderFactory;
		});

		when(mockGatewaySenderFactory.removeGatewayTransportFilter(any(GatewayTransportFilter.class))).thenAnswer(invocation -> {

			gatewayTransportFilters.remove(invocation.<GatewayTransportFilter>getArgument(0));

			return mockGatewaySenderFactory;
		});

		when(mockGatewaySenderFactory.create(anyString(), anyInt())).thenAnswer(invocation -> {

			GatewaySender mockGatewaySender = mock(GatewaySender.class);

			AtomicBoolean destroyed = new AtomicBoolean(false);
			AtomicBoolean running = new AtomicBoolean(false);

			Integer remoteDistributedSystemId = invocation.getArgument(1);

			String gatewaySenderId = invocation.getArgument(0);

			when(mockGatewaySender.isBatchConflationEnabled()).thenAnswer(newGetter(batchConflationEnabled));
			when(mockGatewaySender.isDiskSynchronous()).thenAnswer(newGetter(diskSynchronous));
			when(mockGatewaySender.isParallel()).thenAnswer(newGetter(parallel));
			when(mockGatewaySender.isPaused()).thenAnswer(newGetter(running));
			when(mockGatewaySender.isPersistenceEnabled()).thenAnswer(newGetter(persistenceEnabled));
			when(mockGatewaySender.isRunning()).thenAnswer(newGetter(running));

			when(mockGatewaySender.getAlertThreshold()).thenAnswer(newGetter(alertThreshold));
			when(mockGatewaySender.getBatchSize()).thenAnswer(newGetter(batchSize));
			when(mockGatewaySender.getBatchTimeInterval()).thenAnswer(newGetter(batchTimeInterval));
			when(mockGatewaySender.getDiskStoreName()).thenAnswer(newGetter(diskStoreName));
			when(mockGatewaySender.getDispatcherThreads()).thenAnswer(newGetter(dispatcherThreads));
			when(mockGatewaySender.getGatewayEventFilters()).thenReturn(gatewayEventFilters);
			when(mockGatewaySender.getGatewayEventSubstitutionFilter()).thenAnswer(newGetter(gatewayEventSubstitutionFilter));
			when(mockGatewaySender.getGatewayTransportFilters()).thenReturn(gatewayTransportFilters);
			when(mockGatewaySender.getId()).thenReturn(gatewaySenderId);
			when(mockGatewaySender.getMaximumQueueMemory()).thenAnswer(newGetter(maximumQueueMemory));
			when(mockGatewaySender.getMaxParallelismForReplicatedRegion()).thenAnswer(newGetter(parallelFactorForReplicatedRegion));
			when(mockGatewaySender.getOrderPolicy()).thenAnswer(newGetter(orderPolicy));
			when(mockGatewaySender.getRemoteDSId()).thenReturn(remoteDistributedSystemId);
			when(mockGatewaySender.getSocketBufferSize()).thenAnswer(newGetter(socketBufferSize));
			when(mockGatewaySender.getSocketReadTimeout()).thenAnswer(newGetter(socketReadTimeout));

			doAnswer(it -> {

				gatewayEventFilters.add(it.getArgument(0));

				return null;

			}).when(mockGatewaySender).addGatewayEventFilter(any(GatewayEventFilter.class));

			doAnswer(newSetter(destroyed, null)).when(mockGatewaySender).destroy();
			doAnswer(newSetter(running, false, null)).when(mockGatewaySender).pause();
			doAnswer(newSetter(running, true, null)).when(mockGatewaySender).resume();
			doAnswer(newSetter(running, true, null)).when(mockGatewaySender).start();
			doAnswer(newSetter(running, false, null)).when(mockGatewaySender).stop();

			doAnswer(it -> {

				gatewayEventFilters.remove(it.<GatewayEventFilter>getArgument(0));

				return null;

			}).when(mockGatewaySender).removeGatewayEventFilter(any(GatewayEventFilter.class));

			return mockGatewaySender;
		});

		return mockGatewaySenderFactory;
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
		Set<Index> indexes = new ConcurrentHashSet<>();

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

			when(mockQueryService.getIndexes()).thenReturn(indexes);

			when(mockQueryService.getIndexes(any(Region.class))).thenAnswer(invocation -> {

				Region<?, ?> region = invocation.getArgument(0);

				return indexes.stream()
					.filter(index -> index.getRegion().equals(region))
					.collect(Collectors.toList());

			});

			when(mockQueryService.getIndex(any(Region.class), anyString())).thenAnswer(invocation -> {

				Region<?, ?> region = invocation.getArgument(0);

				String indexName = invocation.getArgument(1);

				Collection<Index> indexesForRegion = mockQueryService.getIndexes(region);

				return indexesForRegion.stream()
					.filter(index -> index.getName().equals(indexName))
					.findFirst().orElse(null);

			});

			when(mockQueryService.createIndex(anyString(), anyString(), anyString()))
				.thenAnswer(createIndexAnswer(indexes, IndexType.FUNCTIONAL));

			when(mockQueryService.createIndex(anyString(), anyString(), anyString(), anyString()))
				.thenAnswer(createIndexAnswer(indexes, IndexType.FUNCTIONAL));

			when(mockQueryService.createHashIndex(anyString(), anyString(), anyString()))
				.thenAnswer(createIndexAnswer(indexes, IndexType.HASH));

			when(mockQueryService.createHashIndex(anyString(), anyString(), anyString(), anyString()))
				.thenAnswer(createIndexAnswer(indexes, IndexType.HASH));

			when(mockQueryService.createKeyIndex(anyString(), anyString(), anyString()))
				.thenAnswer(createIndexAnswer(indexes, IndexType.KEY));

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

	private static Index add(Collection<Index> indexes, Index index) {

		indexes.add(index);

		return index;
	}

	private static Answer<Index> createIndexAnswer(Collection<Index> indexes, IndexType indexType) {

		return invocation -> {

			String indexName = invocation.getArgument(0);
			String indexedExpression = invocation.getArgument(1);
			String regionPath = invocation.getArgument(2);

			return add(indexes, mockIndex(indexName, indexedExpression, regionPath, indexType));
		};
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

	public static Index mockIndex(String name, String expression, String fromClause, IndexType indexType) {

		Index mockIndex = mock(Index.class, name);

		IndexStatistics mockIndexStaticts = mockIndexStatistics(name);

		when(mockIndex.getName()).thenReturn(name);
		when(mockIndex.getCanonicalizedFromClause()).thenReturn(fromClause);
		when(mockIndex.getCanonicalizedIndexedExpression()).thenReturn(expression);
		when(mockIndex.getCanonicalizedProjectionAttributes()).thenReturn(expression);
		when(mockIndex.getFromClause()).thenReturn(fromClause);
		when(mockIndex.getIndexedExpression()).thenReturn(expression);
		when(mockIndex.getProjectionAttributes()).thenReturn(expression);
		when(mockIndex.getRegion()).thenAnswer(invocation -> regions.get(fromClause));
		when(mockIndex.getStatistics()).thenReturn(mockIndexStaticts);
		when(mockIndex.getType()).thenReturn(indexType.getGemfireIndexType());

		return mockIndex;
	}

	private static IndexStatistics mockIndexStatistics(String name) {
		return mock(IndexStatistics.class, mockObjectIdentifier(name));
	}

	@SuppressWarnings("unchecked")
	public static <K, V> Region<K, V> mockRegion(RegionService regionService, String name,
		RegionAttributes<K, V> regionAttributes) {

		Map<K, V> data = new ConcurrentHashMap<>();

		Region<K, V> mockRegion = mock(Region.class, name);

		Set<Region<?, ?>> subRegions = new CopyOnWriteArraySet<>();

		when(mockRegion.getAttributes()).thenAnswer(invocation -> mockRegionAttributes(mockRegion, regionAttributes));
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
			data.entrySet().stream().filter(entry -> entry.getKey().equals(invocation.getArgument(0))).findFirst()
				.orElse(null));

		when(mockRegion.put(any(), any())).thenAnswer(invocation ->
			data.put(invocation.getArgument(0), invocation.getArgument(1)));

		when(mockRegion.size()).thenAnswer(invocation -> data.size());

		when(mockRegion.subregions(anyBoolean())).thenAnswer(invocation -> {

			boolean recursive = invocation.getArgument(0);

			return recursive
				? subRegions.stream()
				.flatMap(subRegion -> subRegion.subregions(true).stream())
				.collect(Collectors.toSet())
				: subRegions;
		});

		return rememberMockedRegion(mockRegion);
	}

	@SuppressWarnings("unchecked")
	private static <K, V> RegionAttributes<K, V> mockRegionAttributes(Region<K, V> mockRegion,
		RegionAttributes<K, V> baseRegionAttributes) {

		AttributesMutator<K, V> mockAttributesMutator = mock(AttributesMutator.class);

		EvictionAttributesMutator mockEvictionAttributesMutator = mock(EvictionAttributesMutator.class);

		RegionAttributes<K, V> mockRegionAttributes = mock(RegionAttributes.class);

		when(mockRegion.getAttributesMutator()).thenReturn(mockAttributesMutator);
		when(mockAttributesMutator.getEvictionAttributesMutator()).thenReturn(mockEvictionAttributesMutator);
		when(mockAttributesMutator.getRegion()).thenReturn(mockRegion);

		AtomicInteger evictionMaximum =
			new AtomicInteger(Optional.ofNullable(baseRegionAttributes.getEvictionAttributes())
				.map(EvictionAttributes::getMaximum)
				.orElse(EvictionAttributes.DEFAULT_ENTRIES_MAXIMUM));

		AtomicReference<Boolean> cloningEnabled = new AtomicReference<>(null);

		AtomicReference<CacheLoader<K, V>> cacheLoader = new AtomicReference<>(baseRegionAttributes.getCacheLoader());

		AtomicReference<CacheWriter<K, V>> cacheWriter = new AtomicReference<>(baseRegionAttributes.getCacheWriter());

		AtomicReference<CustomExpiry<K, V>> customEntryIdleTimeout =
			new AtomicReference<>(baseRegionAttributes.getCustomEntryIdleTimeout());

		AtomicReference<CustomExpiry<K, V>> customEntryTimeToLive =
			new AtomicReference<>(baseRegionAttributes.getCustomEntryTimeToLive());

		AtomicReference<ExpirationAttributes> entryIdleTimeout =
			new AtomicReference<>(baseRegionAttributes.getEntryIdleTimeout());

		AtomicReference<ExpirationAttributes> entryTimeToLive =
			new AtomicReference<>(baseRegionAttributes.getEntryTimeToLive());

		AtomicReference<ExpirationAttributes> regionIdleTimeout =
			new AtomicReference<>(baseRegionAttributes.getRegionIdleTimeout());

		AtomicReference<ExpirationAttributes> regionTimeToLive =
			new AtomicReference<>(baseRegionAttributes.getRegionTimeToLive());

		List<String> asyncEventQueueIds =
			new CopyOnWriteArrayList<>(nullSafeSet(baseRegionAttributes.getAsyncEventQueueIds()));

		List<CacheListener<K, V>> cacheListeners =
			new CopyOnWriteArrayList<>(nullSafeArray(baseRegionAttributes.getCacheListeners(), CacheListener.class));

		List<String> gatewaySenderIds =
			new CopyOnWriteArrayList<>(nullSafeSet(baseRegionAttributes.getGatewaySenderIds()));

		// Mock AttributesMutator
		doAnswer(newAdder(asyncEventQueueIds, null))
			.when(mockAttributesMutator).addAsyncEventQueueId(anyString());

		doAnswer(newAdder(cacheListeners, null))
			.when(mockAttributesMutator).addCacheListener(any(CacheListener.class));

		doAnswer(newAdder(gatewaySenderIds, null)).
			when(mockAttributesMutator).addGatewaySenderId(anyString());

		when(mockAttributesMutator.getCloningEnabled()).thenAnswer(newGetter(() ->
			Optional.ofNullable(cloningEnabled.get()).orElseGet(baseRegionAttributes::getCloningEnabled)));

		doAnswer(invocation -> {

			CacheListener<K, V>[] cacheListenersArgument =
				nullSafeArray(invocation.getArgument(0), CacheListener.class);

			Arrays.stream(cacheListenersArgument).forEach(it ->
				Assert.notNull(it, "The CacheListener[] must not contain null elements"));

			cacheListeners.forEach(CacheListener::close);
			cacheListeners.addAll(Arrays.asList(cacheListenersArgument));

			return null;

		}).when(mockAttributesMutator).initCacheListeners(any(CacheListener[].class));


		doAnswer(invocation -> asyncEventQueueIds.remove(invocation.getArgument(0)))
			.when(mockAttributesMutator).removeAsyncEventQueueId(anyString());

		doAnswer(invocation -> cacheListeners.remove(invocation.getArgument(0)))
			.when(mockAttributesMutator).removeCacheListener(any(CacheListener.class));

		doAnswer(invocation -> gatewaySenderIds.remove(invocation.getArgument(0)))
			.when(mockAttributesMutator).removeGatewaySenderId(anyString());

		doAnswer(newSetter(cacheLoader, baseRegionAttributes.getCacheLoader()))
			.when(mockAttributesMutator).setCacheLoader(any(CacheLoader.class));

		doAnswer(newSetter(cacheWriter, baseRegionAttributes.getCacheWriter()))
			.when(mockAttributesMutator).setCacheWriter(any(CacheWriter.class));

		doAnswer(newSetter(cloningEnabled, null))
			.when(mockAttributesMutator).setCloningEnabled(anyBoolean());

		doAnswer(newSetter(customEntryIdleTimeout, baseRegionAttributes.getCustomEntryIdleTimeout()))
			.when(mockAttributesMutator).setCustomEntryIdleTimeout(any(CustomExpiry.class));

		doAnswer(newSetter(customEntryTimeToLive, baseRegionAttributes.getCustomEntryTimeToLive()))
			.when(mockAttributesMutator).setCustomEntryTimeToLive(any(CustomExpiry.class));

		doAnswer(newSetter(entryIdleTimeout, baseRegionAttributes.getEntryIdleTimeout()))
			.when(mockAttributesMutator).setEntryIdleTimeout(any(ExpirationAttributes.class));

		doAnswer(newSetter(entryTimeToLive, baseRegionAttributes.getEntryTimeToLive()))
			.when(mockAttributesMutator).setEntryTimeToLive(any(ExpirationAttributes.class));

		doAnswer(newSetter(regionIdleTimeout, baseRegionAttributes.getRegionIdleTimeout()))
			.when(mockAttributesMutator).setRegionIdleTimeout(any(ExpirationAttributes.class));

		doAnswer(newSetter(regionTimeToLive, baseRegionAttributes.getRegionTimeToLive()))
			.when(mockAttributesMutator).setRegionTimeToLive(any(ExpirationAttributes.class));

		// Mock EvictionAttributesMutator
		doAnswer(newSetter(evictionMaximum, null)).when(mockEvictionAttributesMutator).setMaximum(anyInt());

		// Mock RegionAttributes
		when(mockRegionAttributes.getAsyncEventQueueIds())
			.thenAnswer(invocation -> asSet(asyncEventQueueIds.toArray(new String[asyncEventQueueIds.size()])));

		when(mockRegionAttributes.getCacheListeners())
			.thenAnswer(invocation -> cacheListeners.toArray(new CacheListener[cacheListeners.size()]));

		when(mockRegionAttributes.getCacheLoader()).thenAnswer(newGetter(cacheLoader::get));
		when(mockRegionAttributes.getCacheWriter()).thenAnswer(newGetter(cacheWriter::get));
		when(mockRegionAttributes.getCloningEnabled()).thenAnswer(newGetter(cloningEnabled::get));
		when(mockRegionAttributes.getCompressor()).thenAnswer(newGetter(baseRegionAttributes::getCompressor));
		when(mockRegionAttributes.getConcurrencyChecksEnabled()).thenAnswer(newGetter(baseRegionAttributes::getConcurrencyChecksEnabled));
		when(mockRegionAttributes.getConcurrencyLevel()).thenAnswer(newGetter(baseRegionAttributes::getConcurrencyLevel));
		when(mockRegionAttributes.getCustomEntryIdleTimeout()).thenAnswer(newGetter(customEntryIdleTimeout::get));
		when(mockRegionAttributes.getCustomEntryTimeToLive()).thenAnswer(newGetter(customEntryTimeToLive::get));
		when(mockRegionAttributes.getDataPolicy()).thenAnswer(newGetter(baseRegionAttributes::getDataPolicy));
		when(mockRegionAttributes.getDiskStoreName()).thenAnswer(newGetter(baseRegionAttributes::getDiskStoreName));
		when(mockRegionAttributes.getEnableAsyncConflation()).thenAnswer(newGetter(baseRegionAttributes::getEnableAsyncConflation));
		when(mockRegionAttributes.getEnableSubscriptionConflation()).thenAnswer(newGetter(baseRegionAttributes::getEnableSubscriptionConflation));
		when(mockRegionAttributes.getEntryIdleTimeout()).thenAnswer(newGetter(entryIdleTimeout::get));
		when(mockRegionAttributes.getEntryTimeToLive()).thenAnswer(newGetter(entryTimeToLive::get));

		when(mockRegionAttributes.getEvictionAttributes()).thenAnswer(invocation -> {

			EvictionAttributes mockEvictionAttibutes = mock(EvictionAttributes.class);
			EvictionAttributes regionEvictionAttributes = baseRegionAttributes.getEvictionAttributes();

			when(mockEvictionAttibutes.getAction()).thenAnswer(newGetter(regionEvictionAttributes::getAction));
			when(mockEvictionAttibutes.getAlgorithm()).thenAnswer(newGetter(regionEvictionAttributes::getAlgorithm));
			when(mockEvictionAttibutes.getMaximum()).thenAnswer(newGetter(evictionMaximum));
			when(mockEvictionAttibutes.getObjectSizer()).thenAnswer(newGetter(regionEvictionAttributes::getObjectSizer));

			return mockEvictionAttibutes;
		});

		when(mockRegionAttributes.getGatewaySenderIds())
			.thenAnswer(invocation -> asSet(gatewaySenderIds.toArray(new String[gatewaySenderIds.size()])));

		when(mockRegionAttributes.getIgnoreJTA()).thenAnswer(newGetter(baseRegionAttributes::getIgnoreJTA));
		when(mockRegionAttributes.getIndexMaintenanceSynchronous()).thenAnswer(newGetter(baseRegionAttributes::getIndexMaintenanceSynchronous));
		when(mockRegionAttributes.getInitialCapacity()).thenAnswer(newGetter(baseRegionAttributes::getInitialCapacity));
		when(mockRegionAttributes.getKeyConstraint()).thenAnswer(newGetter(baseRegionAttributes::getKeyConstraint));
		when(mockRegionAttributes.getLoadFactor()).thenAnswer(newGetter(baseRegionAttributes::getLoadFactor));
		when(mockRegionAttributes.getMulticastEnabled()).thenAnswer(newGetter(baseRegionAttributes::getMulticastEnabled));
		when(mockRegionAttributes.getOffHeap()).thenAnswer(newGetter(baseRegionAttributes::getOffHeap));
		when(mockRegionAttributes.getPartitionAttributes()).thenAnswer(newGetter(baseRegionAttributes::getPartitionAttributes));
		when(mockRegionAttributes.getPoolName()).thenAnswer(newGetter(baseRegionAttributes::getPoolName));
		when(mockRegionAttributes.getRegionIdleTimeout()).thenAnswer(newGetter(regionIdleTimeout::get));
		when(mockRegionAttributes.getRegionTimeToLive()).thenAnswer(newGetter(regionTimeToLive::get));
		when(mockRegionAttributes.getScope()).thenAnswer(newGetter(baseRegionAttributes::getScope));
		when(mockRegionAttributes.getStatisticsEnabled()).thenAnswer(newGetter(baseRegionAttributes::getStatisticsEnabled));
		when(mockRegionAttributes.getSubscriptionAttributes()).thenAnswer(newGetter(baseRegionAttributes::getSubscriptionAttributes));
		when(mockRegionAttributes.getValueConstraint()).thenAnswer(newGetter(baseRegionAttributes::getValueConstraint));
		when(mockRegionAttributes.isDiskSynchronous()).thenAnswer(newGetter(baseRegionAttributes::isDiskSynchronous));
		when(mockRegionAttributes.isLockGrantor()).thenAnswer(newGetter(baseRegionAttributes::isLockGrantor));

		return mockRegionAttributes;
	}

	public static <K, V> Region<K, V> mockSubRegion(Region<K, V> parent, String name,
		RegionAttributes<K, V> regionAttributes) {

		String subRegionName = String.format("%1$s%2$s", parent.getFullPath(), toRegionPath(name));

		Region<K, V> mockSubRegion = mockRegion(parent.getRegionService(), subRegionName, regionAttributes);

		parent.subregions(false).add(mockSubRegion);

		return mockSubRegion;
	}

	public static <K, V> RegionFactory<K, V> mockRegionFactory(Cache mockCache) {
		return mockRegionFactory(mockCache, null, null);
	}

	public static <K, V> RegionFactory<K, V> mockRegionFactory(Cache mockCache,
		RegionAttributes<K, V> regionAttributes) {

		return mockRegionFactory(mockCache, null, regionAttributes);
	}

	public static <K, V> RegionFactory<K, V> mockRegionFactory(Cache mockCache, RegionShortcut regionShortcut) {
		return mockRegionFactory(mockCache, regionShortcut, null);
	}

	public static <K, V> RegionFactory<K, V> mockRegionFactory(Cache mockCache, String regionAttributesId) {
		return mockRegionFactory(mockCache, null, resolveRegionAttributes(regionAttributesId));
	}

	@SuppressWarnings("unchecked")
	public static <K, V> RegionFactory<K, V> mockRegionFactory(Cache mockCache, RegionShortcut regionShortcut,
		RegionAttributes<K, V> regionAttributes) {

		RegionFactory<K, V> mockRegionFactory = mock(RegionFactory.class,
			mockObjectIdentifier("MockRegionFactory"));

		Optional<RegionAttributes<K, V>> optionalRegionAttributes = Optional.ofNullable(regionAttributes);

		ExpirationAttributes DEFAULT_EXPIRATION_ATTRIBUTES =
			new ExpirationAttributes(0, ExpirationAction.INVALIDATE);

		AtomicBoolean cloningEnabled = new AtomicBoolean(optionalRegionAttributes
			.map(RegionAttributes::getCloningEnabled).orElse(false));

		AtomicBoolean concurrencyChecksEnabled = new AtomicBoolean(optionalRegionAttributes
			.map(RegionAttributes::getConcurrencyChecksEnabled).orElse(true));

		AtomicBoolean diskSynchronous = new AtomicBoolean(optionalRegionAttributes
			.map(RegionAttributes::isDiskSynchronous).orElse(true));

		AtomicBoolean enableAsyncConflation = new AtomicBoolean(optionalRegionAttributes
			.map(RegionAttributes::getEnableAsyncConflation).orElse(false));

		AtomicBoolean enableSubscriptionConflation = new AtomicBoolean(optionalRegionAttributes
			.map(RegionAttributes::getEnableSubscriptionConflation).orElse(false));

		AtomicBoolean ignoreJta = new AtomicBoolean(optionalRegionAttributes
			.map(RegionAttributes::getIgnoreJTA).orElse(false));

		AtomicBoolean indexMaintenanceSynchronous = new AtomicBoolean(optionalRegionAttributes
			.map(RegionAttributes::getIndexMaintenanceSynchronous).orElse(true));

		AtomicBoolean lockGrantor = new AtomicBoolean(optionalRegionAttributes
			.map(RegionAttributes::isLockGrantor).orElse(false));

		AtomicBoolean multicastEnabled = new AtomicBoolean(optionalRegionAttributes
			.map(RegionAttributes::getMulticastEnabled).orElse(false));

		AtomicBoolean offHeap = new AtomicBoolean(optionalRegionAttributes
			.map(RegionAttributes::getOffHeap).orElse(false));

		AtomicBoolean statisticsEnabled = new AtomicBoolean(optionalRegionAttributes
			.map(RegionAttributes::getStatisticsEnabled).orElse(false));

		AtomicInteger concurrencyLevel = new AtomicInteger(optionalRegionAttributes
			.map(RegionAttributes::getConcurrencyLevel).orElse(16));

		AtomicInteger initialCapacity = new AtomicInteger(optionalRegionAttributes
			.map(RegionAttributes::getInitialCapacity).orElse(16));

		AtomicReference<CacheLoader> cacheLoader = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getCacheLoader).orElse(null));

		AtomicReference<CacheWriter> cacheWriter = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getCacheWriter).orElse(null));

		AtomicReference<Compressor> compressor = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getCompressor).orElse(null));

		AtomicReference<CustomExpiry<K, V>> customEntryIdleTimeout = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getCustomEntryIdleTimeout).orElse(null));

		AtomicReference<CustomExpiry<K, V>> customEntryTimeToLive = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getCustomEntryTimeToLive).orElse(null));

		AtomicReference<DataPolicy> dataPolicy = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getDataPolicy).orElseGet(() -> convert(regionShortcut)));

		AtomicReference<String> diskStoreName = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getDiskStoreName).orElse(null));

		AtomicReference<ExpirationAttributes> entryIdleTimeout = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getEntryIdleTimeout).orElse(DEFAULT_EXPIRATION_ATTRIBUTES));

		AtomicReference<ExpirationAttributes> entryTimeToLive = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getEntryTimeToLive).orElse(DEFAULT_EXPIRATION_ATTRIBUTES));

		AtomicReference<EvictionAttributes> evictionAttributes = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getEvictionAttributes).orElseGet(EvictionAttributes::createLRUEntryAttributes));

		AtomicReference<Class<K>> keyConstraint = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getKeyConstraint).orElse(null));

		AtomicReference<Float> loadFactor = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getLoadFactor).orElse(0.75f));

		AtomicReference<PartitionAttributes<K, V>> partitionAttributes = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getPartitionAttributes).orElse(null));

		AtomicReference<String> poolName = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getPoolName).orElse(null));

		AtomicReference<ExpirationAttributes> regionIdleTimeout = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getRegionIdleTimeout).orElse(DEFAULT_EXPIRATION_ATTRIBUTES));

		AtomicReference<ExpirationAttributes> regionTimeToLive = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getRegionTimeToLive).orElse(DEFAULT_EXPIRATION_ATTRIBUTES));

		AtomicReference<Scope> scope = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getScope).orElse(Scope.DISTRIBUTED_NO_ACK));

		AtomicReference<SubscriptionAttributes> subscriptionAttributes = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getSubscriptionAttributes).orElseGet(SubscriptionAttributes::new));

		AtomicReference<Class<V>> valueConstraint = new AtomicReference<>(optionalRegionAttributes
			.map(RegionAttributes::getValueConstraint).orElse(null));

		List<CacheListener> cacheListeners = new ArrayList<>(Arrays.asList(nullSafeArray(optionalRegionAttributes
			.map(RegionAttributes::getCacheListeners).orElse(null), CacheListener.class)));

		Set<String> asyncEventQueueIds = new HashSet<>(nullSafeSet(optionalRegionAttributes
			.map(RegionAttributes::getAsyncEventQueueIds).orElse(null)));

		Set<String> gatewaySenderIds = new HashSet<>(nullSafeSet(optionalRegionAttributes
			.map(RegionAttributes::getGatewaySenderIds).orElse(null)));

		when(mockRegionFactory.addAsyncEventQueueId(anyString()))
			.thenAnswer(newAdder(asyncEventQueueIds, mockRegionFactory));

		when(mockRegionFactory.addCacheListener(any(CacheListener.class)))
			.thenAnswer(newAdder(cacheListeners, mockRegionFactory));

		when(mockRegionFactory.addGatewaySenderId(anyString()))
			.thenAnswer(newAdder(gatewaySenderIds, mockRegionFactory));

		when(mockRegionFactory.initCacheListeners(any(CacheListener[].class))).thenAnswer(invocation -> {
			cacheListeners.clear();
			Collections.addAll(cacheListeners, invocation.getArgument(0));
			return mockRegionFactory;
		});

		when(mockRegionFactory.setCacheLoader(any(CacheLoader.class)))
			.thenAnswer(newSetter(cacheLoader, mockRegionFactory));

		when(mockRegionFactory.setCacheWriter(any(CacheWriter.class)))
			.thenAnswer(newSetter(cacheWriter, mockRegionFactory));

		when(mockRegionFactory.setCloningEnabled(anyBoolean()))
			.thenAnswer(newSetter(cloningEnabled, mockRegionFactory));

		when(mockRegionFactory.setCompressor(any(Compressor.class)))
			.thenAnswer(newSetter(compressor, mockRegionFactory));

		when(mockRegionFactory.setConcurrencyChecksEnabled(anyBoolean()))
			.then(newSetter(concurrencyChecksEnabled, mockRegionFactory));

		when(mockRegionFactory.setConcurrencyLevel(anyInt()))
			.thenAnswer(newSetter(concurrencyLevel, mockRegionFactory));

		when(mockRegionFactory.setCustomEntryIdleTimeout(any(CustomExpiry.class)))
			.thenAnswer(newSetter(customEntryIdleTimeout, mockRegionFactory));

		when(mockRegionFactory.setCustomEntryTimeToLive(any(CustomExpiry.class)))
			.thenAnswer(newSetter(customEntryTimeToLive, mockRegionFactory));

		when(mockRegionFactory.setDataPolicy(any(DataPolicy.class)))
			.thenAnswer(newSetter(dataPolicy, mockRegionFactory));

		when(mockRegionFactory.setDiskStoreName(anyString())).thenAnswer(newSetter(diskStoreName, mockRegionFactory));

		when(mockRegionFactory.setDiskSynchronous(anyBoolean()))
			.thenAnswer(newSetter(diskSynchronous, mockRegionFactory));

		when(mockRegionFactory.setEnableAsyncConflation(anyBoolean()))
			.thenAnswer(newSetter(enableAsyncConflation, mockRegionFactory));

		when(mockRegionFactory.setEnableSubscriptionConflation(anyBoolean()))
			.thenAnswer(newSetter(enableSubscriptionConflation, mockRegionFactory));

		when(mockRegionFactory.setEntryIdleTimeout(any(ExpirationAttributes.class)))
			.thenAnswer(newSetter(entryIdleTimeout, mockRegionFactory));

		when(mockRegionFactory.setEntryTimeToLive(any(ExpirationAttributes.class)))
			.thenAnswer(newSetter(entryTimeToLive, mockRegionFactory));

		when(mockRegionFactory.setEvictionAttributes(any(EvictionAttributes.class)))
			.thenAnswer(newSetter(evictionAttributes, mockRegionFactory));

		when(mockRegionFactory.setIgnoreJTA(anyBoolean())).thenAnswer(newSetter(ignoreJta, mockRegionFactory));

		when(mockRegionFactory.setIndexMaintenanceSynchronous(anyBoolean()))
			.thenAnswer(newSetter(indexMaintenanceSynchronous, mockRegionFactory));

		when(mockRegionFactory.setInitialCapacity(anyInt())).thenAnswer(newSetter(initialCapacity, mockRegionFactory));

		when(mockRegionFactory.setKeyConstraint(any(Class.class)))
			.thenAnswer(newSetter(keyConstraint, mockRegionFactory));

		when(mockRegionFactory.setLoadFactor(anyFloat())).thenAnswer(newSetter(loadFactor, mockRegionFactory));

		when(mockRegionFactory.setLockGrantor(anyBoolean())).thenAnswer(newSetter(lockGrantor, mockRegionFactory));

		when(mockRegionFactory.setMulticastEnabled(anyBoolean()))
			.thenAnswer(newSetter(multicastEnabled, mockRegionFactory));

		when(mockRegionFactory.setOffHeap(anyBoolean())).thenAnswer(newSetter(offHeap, mockRegionFactory));

		when(mockRegionFactory.setPartitionAttributes(any(PartitionAttributes.class)))
			.thenAnswer(newSetter(partitionAttributes, mockRegionFactory));

		when(mockRegionFactory.setPoolName(anyString())).thenAnswer(newSetter(poolName, mockRegionFactory));

		when(mockRegionFactory.setRegionIdleTimeout(any(ExpirationAttributes.class)))
			.thenAnswer(newSetter(regionIdleTimeout, mockRegionFactory));

		when(mockRegionFactory.setRegionTimeToLive(any(ExpirationAttributes.class)))
			.thenAnswer(newSetter(regionTimeToLive, mockRegionFactory));

		when(mockRegionFactory.setScope(any(Scope.class))).thenAnswer(newSetter(scope, mockRegionFactory));

		when(mockRegionFactory.setStatisticsEnabled(anyBoolean()))
			.thenAnswer(newSetter(statisticsEnabled, mockRegionFactory));

		when(mockRegionFactory.setSubscriptionAttributes(any(SubscriptionAttributes.class)))
			.thenAnswer(newSetter(subscriptionAttributes, mockRegionFactory));

		when(mockRegionFactory.setValueConstraint(any(Class.class)))
			.thenAnswer(newSetter(valueConstraint, mockRegionFactory));

		RegionAttributes<K, V> mockRegionAttributes =
			mock(RegionAttributes.class, mockObjectIdentifier("MockRegionAttributes"));

		when(mockRegionAttributes.getAsyncEventQueueIds()).thenReturn(asyncEventQueueIds);

		when(mockRegionAttributes.getCacheListeners())
			.thenAnswer(newGetter(() -> cacheListeners.toArray(new CacheListener[cacheListeners.size()])));

		when(mockRegionAttributes.getCacheLoader()).thenAnswer(newGetter(cacheLoader));
		when(mockRegionAttributes.getCacheWriter()).thenAnswer(newGetter(cacheWriter));
		when(mockRegionAttributes.getCloningEnabled()).thenAnswer(newGetter(cloningEnabled));
		when(mockRegionAttributes.getCompressor()).thenAnswer(newGetter(compressor));
		when(mockRegionAttributes.getConcurrencyChecksEnabled()).thenAnswer(newGetter(concurrencyChecksEnabled));
		when(mockRegionAttributes.getConcurrencyLevel()).thenAnswer(newGetter(concurrencyLevel));
		when(mockRegionAttributes.getCustomEntryIdleTimeout()).thenAnswer(newGetter(customEntryIdleTimeout));
		when(mockRegionAttributes.getCustomEntryTimeToLive()).thenAnswer(newGetter(customEntryTimeToLive));
		when(mockRegionAttributes.getDataPolicy()).thenAnswer(newGetter(dataPolicy));
		when(mockRegionAttributes.getDiskStoreName()).thenAnswer(newGetter(diskStoreName));
		when(mockRegionAttributes.isDiskSynchronous()).thenAnswer(newGetter(diskSynchronous));
		when(mockRegionAttributes.getEnableAsyncConflation()).thenAnswer(newGetter(enableAsyncConflation));
		when(mockRegionAttributes.getEnableSubscriptionConflation()).thenAnswer(newGetter(enableSubscriptionConflation));
		when(mockRegionAttributes.getEntryIdleTimeout()).thenAnswer(newGetter(entryIdleTimeout));
		when(mockRegionAttributes.getEntryTimeToLive()).thenAnswer(newGetter(entryTimeToLive));
		when(mockRegionAttributes.getEvictionAttributes()).thenAnswer(newGetter(evictionAttributes));
		when(mockRegionAttributes.getGatewaySenderIds()).thenReturn(gatewaySenderIds);
		when(mockRegionAttributes.getIgnoreJTA()).thenAnswer(newGetter(ignoreJta));
		when(mockRegionAttributes.getIndexMaintenanceSynchronous()).thenAnswer(newGetter(indexMaintenanceSynchronous));
		when(mockRegionAttributes.getInitialCapacity()).thenAnswer(newGetter(initialCapacity));
		when(mockRegionAttributes.getKeyConstraint()).thenAnswer(newGetter(keyConstraint));
		when(mockRegionAttributes.getLoadFactor()).thenAnswer(newGetter(loadFactor));
		when(mockRegionAttributes.isLockGrantor()).thenAnswer(newGetter(lockGrantor));
		when(mockRegionAttributes.getMulticastEnabled()).thenAnswer(newGetter(multicastEnabled));
		when(mockRegionAttributes.getOffHeap()).thenAnswer(newGetter(offHeap));
		when(mockRegionAttributes.getPartitionAttributes()).thenAnswer(newGetter(partitionAttributes));
		when(mockRegionAttributes.getPoolName()).thenAnswer(newGetter(poolName));
		when(mockRegionAttributes.getRegionIdleTimeout()).thenAnswer(newGetter(regionIdleTimeout));
		when(mockRegionAttributes.getRegionTimeToLive()).thenAnswer(newGetter(regionTimeToLive));
		when(mockRegionAttributes.getScope()).thenAnswer(newGetter(scope));
		when(mockRegionAttributes.getStatisticsEnabled()).thenAnswer(newGetter(statisticsEnabled));
		when(mockRegionAttributes.getSubscriptionAttributes()).thenAnswer(newGetter(subscriptionAttributes));
		when(mockRegionAttributes.getValueConstraint()).thenAnswer(newGetter(valueConstraint));

		when(mockRegionFactory.create(anyString())).thenAnswer(invocation ->
			mockRegion(mockCache, invocation.getArgument(0), mockRegionAttributes));

		when(mockRegionFactory.createSubregion(any(Region.class), anyString())).thenAnswer(invocation ->
			mockSubRegion(invocation.getArgument(0), invocation.getArgument(1), mockRegionAttributes));

		return mockRegionFactory;
	}

	public static ResourceManager mockResourceManager() {

		ResourceManager mockResourceManager = mock(ResourceManager.class);

		AtomicReference<Float> criticalHeapPercentage =
			new AtomicReference<>(ResourceManager.DEFAULT_CRITICAL_PERCENTAGE);

		AtomicReference<Float> criticalOffHeapPercentage = new AtomicReference<>(0.0f);

		AtomicReference<Float> evictionHeapPercentage =
			new AtomicReference<>(ResourceManager.DEFAULT_EVICTION_PERCENTAGE);

		AtomicReference<Float> evictionOffHeapPercentage = new AtomicReference<>(0.0f);

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
		return spyOn(cacheFactory, DEFAULT_USE_SINGLETON_CACHE);
	}

	public static CacheFactory spyOn(CacheFactory cacheFactory, boolean useSingletonCache) {

		CacheFactory cacheFactorySpy = spy(cacheFactory);

		Cache resolvedMockCache = GemFireMockObjectsSupport.<Cache>resolveMockedGemFireCache(useSingletonCache)
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
			GemFireMockObjectsSupport.<ClientCache>resolveMockedGemFireCache(useSingletonCache).orElseGet(() -> {

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

	protected interface IoExceptionThrowingOperation {
		void doIo() throws IOException;
	}
}
