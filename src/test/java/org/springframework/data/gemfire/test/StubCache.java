package org.springframework.data.gemfire.test;

import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.InputStream;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import javax.naming.Context;

import org.apache.geode.CancelCriterion;
import org.apache.geode.LogWriter;
import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheTransactionManager;
import org.apache.geode.cache.CacheWriterException;
import org.apache.geode.cache.Declarable;
import org.apache.geode.cache.DiskStore;
import org.apache.geode.cache.DiskStoreFactory;
import org.apache.geode.cache.GatewayException;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.RegionExistsException;
import org.apache.geode.cache.RegionFactory;
import org.apache.geode.cache.RegionService;
import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.TimeoutException;
import org.apache.geode.cache.asyncqueue.AsyncEventQueue;
import org.apache.geode.cache.asyncqueue.AsyncEventQueueFactory;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientRegionFactory;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.control.ResourceManager;
import org.apache.geode.cache.query.Index;
import org.apache.geode.cache.query.IndexExistsException;
import org.apache.geode.cache.query.IndexInvalidException;
import org.apache.geode.cache.query.IndexNameConflictException;
import org.apache.geode.cache.query.QueryService;
import org.apache.geode.cache.query.RegionNotFoundException;
import org.apache.geode.cache.server.CacheServer;
import org.apache.geode.cache.snapshot.CacheSnapshotService;
import org.apache.geode.cache.util.GatewayConflictResolver;
import org.apache.geode.cache.wan.GatewayReceiver;
import org.apache.geode.cache.wan.GatewayReceiverFactory;
import org.apache.geode.cache.wan.GatewaySender;
import org.apache.geode.cache.wan.GatewaySenderFactory;
import org.apache.geode.distributed.DistributedMember;
import org.apache.geode.distributed.DistributedSystem;
import org.apache.geode.i18n.LogWriterI18n;
import org.apache.geode.pdx.PdxInstance;
import org.apache.geode.pdx.PdxInstanceFactory;
import org.apache.geode.pdx.PdxSerializer;

@SuppressWarnings({ "deprecation", "unused" })
public class StubCache implements Cache, ClientCache {

	private static final AtomicReference<QueryService> queryServiceReference = new AtomicReference<>(null);

	protected static final String NOT_IMPLEMENTED = "Not Implemented";

	private boolean closed;
	private boolean copyOnRead;
	private boolean pdxIgnoreUnreadFields;
	private boolean pdxPersistent;
	private boolean pdxReadSerialized;
	private boolean server;

	private int lockLease;
	private int lockTimeout;
	private int messageSyncInterval;
	private int searchTimeout;

	private CacheTransactionManager cacheTransactionManager;

	private Context jndiContext;

	private Declarable initializer;

	private DistributedSystem distributedSystem;

	private GatewayConflictResolver gatewayConflictResolver;

	private Map<String, Region> rootRegions;

	private LogWriter logWriter;
	private LogWriter securityLogWriter;

	private PdxSerializer pdxSerializer;

	private Properties properties;

	private ResourceManager resourceManager;

	private Set<GatewayReceiver> gatewayReceivers;
	private Set<GatewaySender> gatewaySenders;

	private String pdxDiskStore;
	private String name;

	public StubCache(){
		rootRegions = new HashMap<>();
		resourceManager = new StubResourceManager();
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#createDiskStoreFactory()
	 */
	@Override
	public DiskStoreFactory createDiskStoreFactory() {
		return new StubDiskStore();
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#findDiskStore(java.lang.String)
	 */
	@Override
	public DiskStore findDiskStore(String name) {
		return StubDiskStore.getDiskStore(name);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#getCacheTransactionManager()
	 */
	@Override
	public CacheTransactionManager getCacheTransactionManager() {
		if (cacheTransactionManager == null) {
			cacheTransactionManager = new StubCacheTransactionManager();
		}

		return cacheTransactionManager;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#getCopyOnRead()
	 */
	@Override
	public boolean getCopyOnRead() {
		return this.copyOnRead;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#getDistributedSystem()
	 */
	@Override
	public DistributedSystem getDistributedSystem() {
		if (distributedSystem == null) {
			distributedSystem = mockDistributedSystem();
		}

		return distributedSystem;
	}


	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#getInitializer()
	 */
	@Override
	public Declarable getInitializer() {
		return this.initializer;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#getInitializerProps()
	 */
	@Override
	public Properties getInitializerProps() {
		return this.properties;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#getJNDIContext()
	 */
	@Override
	public Context getJNDIContext() {
		return this.jndiContext;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#getLogger()
	 */
	@Override
	public LogWriter getLogger() {
		return this.logWriter;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#getName()
	 */
	@Override
	public String getName() {
		return (this.properties != null ? this.properties.getProperty("name") : null);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#getPdxDiskStore()
	 */
	@Override
	public String getPdxDiskStore() {
		return this.pdxDiskStore;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#getPdxIgnoreUnreadFields()
	 */
	@Override
	public boolean getPdxIgnoreUnreadFields() {
		return this.pdxIgnoreUnreadFields;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#getPdxPersistent()
	 */
	@Override
	public boolean getPdxPersistent() {
		return this.pdxPersistent;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#getPdxReadSerialized()
	 */
	@Override
	public boolean getPdxReadSerialized() {
		return this.pdxReadSerialized;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#getPdxSerializer()
	 */
	@Override
	public PdxSerializer getPdxSerializer() {
		return this.pdxSerializer;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#getRegionAttributes(java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <K, V> RegionAttributes<K, V> getRegionAttributes(String region) {
		return allRegions().get(region).getAttributes();
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#getResourceManager()
	 */
	@Override
	public ResourceManager getResourceManager() {
		return this.resourceManager;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#getSecurityLogger()
	 */
	@Override
	public LogWriter getSecurityLogger() {
		return this.securityLogWriter;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#listRegionAttributes()
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public <K, V> Map<String, RegionAttributes<K, V>> listRegionAttributes() {

		Map<String, RegionAttributes<K, V>> attributes = new HashMap<>();

		for (Entry<String, Region> entry: allRegions().entrySet()) {
			attributes.put(entry.getKey(), entry.getValue().getAttributes());
		}

		return attributes;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#loadCacheXml(java.io.InputStream)
	 */
	@Override
	public void loadCacheXml(InputStream is) throws TimeoutException, CacheWriterException, GatewayException, RegionExistsException {
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#setCopyOnRead(boolean)
	 */
	@Override
	public void setCopyOnRead(boolean arg0) {
		this.copyOnRead = arg0;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.GemFireCache#setRegionAttributes(java.lang.String, org.apache.geode.cache.RegionAttributes)
	 */
	@Override
	public <K, V> void setRegionAttributes(String region, RegionAttributes<K, V> attributes) {
		RegionFactory<K,V> rf = new MockRegionFactory<K, V>(this).createMockRegionFactory(attributes);
		rf.create(region);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.RegionService#close()
	 */
	@Override
	public void close() {
		this.closed = true;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.RegionService#createPdxEnum(java.lang.String, java.lang.String, int)
	 */
	@Override
	public PdxInstance createPdxEnum(String arg0, String arg1, int arg2) {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.RegionService#createPdxInstanceFactory(java.lang.String)
	 */
	@Override
	public PdxInstanceFactory createPdxInstanceFactory(String arg0) {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.RegionService#getCancelCriterion()
	 */
	@Override
	public CancelCriterion getCancelCriterion() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.RegionService#getQueryService()
	 */
	@Override
	public QueryService getQueryService() {
		try {
			queryServiceReference.compareAndSet(null, mockQueryService());
			return queryServiceReference.get();
		}
		catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.RegionService#getRegion(java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <K, V> Region<K, V> getRegion(String name) {
		return allRegions().get(name);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.RegionService#isClosed()
	 */
	@Override
	public boolean isClosed() {
		return this.closed;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.RegionService#rootRegions()
	 */
	@Override
	public Set<Region<?, ?>> rootRegions() {

		Set<Region<?,?>> rootRegions = new HashSet<>();

		for (String key: allRegions().keySet()) {
			if (!key.contains(Region.SEPARATOR)) {
				rootRegions.add(allRegions().get(key));
			}
		}

		return rootRegions;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#addCacheServer()
	 */
	@Override
	public CacheServer addCacheServer() {
		return mockCacheServer();
	}


	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#close(boolean)
	 */
	@Override
	@Deprecated
	public void close(boolean arg0) {
		this.closed = true;

	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#createAsyncEventQueueFactory()
	 */
	@Override
	public AsyncEventQueueFactory createAsyncEventQueueFactory() {
		return new StubAsyncEventQueueFactory();
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#createGatewayReceiverFactory()
	 */
	@Override
	public GatewayReceiverFactory createGatewayReceiverFactory() {
		return new StubGatewayReceiverFactory();
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#createGatewaySenderFactory()
	 */
	@Override
	public GatewaySenderFactory createGatewaySenderFactory() {
		return new StubGatewaySenderFactory();
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#createRegion(java.lang.String, org.apache.geode.cache.RegionAttributes)
	 */
	@Override
	@Deprecated
	public <K, V> Region<K, V> createRegion(String arg0, RegionAttributes<K, V> arg1) throws RegionExistsException, TimeoutException {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#createRegionFactory()
	 */
	@Override
	@SuppressWarnings("unchecked")
	public <K, V> RegionFactory<K, V> createRegionFactory() {
		return new MockRegionFactory<K,V>(this).createRegionFactory();
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#createRegionFactory(org.apache.geode.cache.RegionAttributes)
	 */
	@Override
	public <K, V> RegionFactory<K, V> createRegionFactory(RegionAttributes<K, V> regionAttributes) {
		return new MockRegionFactory<K,V>(this).createMockRegionFactory(regionAttributes);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#createRegionFactory(org.apache.geode.cache.RegionShortcut)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public <K, V> RegionFactory<K, V> createRegionFactory(RegionShortcut shortcut) {
		return new MockRegionFactory<K,V>(this).createRegionFactory();
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#createRegionFactory(java.lang.String)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public <K, V> RegionFactory<K, V> createRegionFactory(String regionAttributesId) {
		return new MockRegionFactory<K,V>(this).createRegionFactory();
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#createVMRegion(java.lang.String, org.apache.geode.cache.RegionAttributes)
	 */
	@Override
	@Deprecated
	public <K, V> Region<K, V> createVMRegion(String arg0, RegionAttributes<K, V> arg1) throws RegionExistsException, TimeoutException {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#getAdminMembers()
	 */
	@Override
	public Set<DistributedMember> getAdminMembers() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#getAsyncEventQueue(java.lang.String)
	 */
	@Override
	public AsyncEventQueue getAsyncEventQueue(String name) {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#getAsyncEventQueues()
	 */
	@Override
	public Set<AsyncEventQueue> getAsyncEventQueues() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#getCacheServers()
	 */
	@Override
	public List<CacheServer> getCacheServers() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#getGatewayConflictResolver()
	 */
	@Override
	public GatewayConflictResolver getGatewayConflictResolver() {
		return this.gatewayConflictResolver;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#getGatewayReceivers()
	 */
	@Override
	public Set<GatewayReceiver> getGatewayReceivers() {
		return this.gatewayReceivers;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#getGatewaySender(java.lang.String)
	 */
	@Override
	public GatewaySender getGatewaySender(String name) {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#getGatewaySenders()
	 */
	@Override
	public Set<GatewaySender> getGatewaySenders() {
		return this.gatewaySenders;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#getLockLease()
	 */
	@Override
	public int getLockLease() {
		return this.lockLease;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#getLockTimeout()
	 */
	@Override
	public int getLockTimeout() {
		return this.lockTimeout;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#getLoggerI18n()
	 */
	@Override
	@Deprecated
	public LogWriterI18n getLoggerI18n() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
		 * @see org.apache.geode.cache.Cache#getMembers()
		 */
	@Override
	public Set<DistributedMember> getMembers() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#getMembers(org.apache.geode.cache.Region)
	 */
	@SuppressWarnings({"rawtypes"})
	@Override
	public Set<DistributedMember> getMembers(Region arg0) {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#getMessageSyncInterval()
	 */
	@Override
	public int getMessageSyncInterval() {
		return this.messageSyncInterval;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#getSearchTimeout()
	 */
	@Override
	public int getSearchTimeout() {
		return this.searchTimeout;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#getSecurityLoggerI18n()
	 */
	@Override
	@Deprecated
	public LogWriterI18n getSecurityLoggerI18n() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#getSnapshotService()
	 */
	@Override
	public CacheSnapshotService getSnapshotService() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#isServer()
	 */
	@Override
	public boolean isServer() {
		return this.server;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#readyForEvents()
	 */
	@Override
	@Deprecated
	public void readyForEvents() {
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#setGatewayConflictResolver(org.apache.geode.cache.util.GatewayConflictResolver)
	 */
	@Override
	public void setGatewayConflictResolver(GatewayConflictResolver arg0) {
		this.gatewayConflictResolver = arg0;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#setIsServer(boolean)
	 */
	@Override
	public void setIsServer(boolean server) {
		this.server = server;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#setLockLease(int)
	 */
	@Override
	public void setLockLease(int arg0) {
		this.lockLease = arg0;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#setLockTimeout(int)
	 */
	@Override
	public void setLockTimeout(int arg0) {
		this.lockTimeout = arg0;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#setMessageSyncInterval(int)
	 */
	@Override
	public void setMessageSyncInterval(int arg0) {
		this.messageSyncInterval = arg0;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.Cache#setSearchTimeout(int)
	 */
	@Override
	public void setSearchTimeout(int arg0) {
		this.searchTimeout = arg0;
	}

	DistributedSystem mockDistributedSystem() {

		DistributedSystem mockDistributedSystem = mock(DistributedSystem.class);

		when(mockDistributedSystem.getName()).thenAnswer(invocation -> getName());

		when(mockDistributedSystem.getProperties()).thenReturn(this.properties);

		DistributedMember mockDistributedMember = mockDistributedMember();

		when(mockDistributedSystem.getDistributedMember()).thenReturn(mockDistributedMember);

		return mockDistributedSystem;
	}

	DistributedMember mockDistributedMember() {
		DistributedMember mockDistributedMember = mock(DistributedMember.class);
		when(mockDistributedMember.getHost()).thenReturn("mockDistributedMember.host");
		when(mockDistributedMember.getName()).thenReturn("mockDistributedMember");
		return mockDistributedMember;
	}

	CacheServer mockCacheServer() {
		return new StubCacheServer();
	}

	QueryService mockQueryService() throws RegionNotFoundException, IndexInvalidException, IndexNameConflictException,
		IndexExistsException, UnsupportedOperationException {

		QueryService queryService = mock(QueryService.class);

		when(queryService.getIndexes()).thenReturn(new ArrayList<>());

		when(queryService.createIndex(anyString(), anyString(),anyString())).thenAnswer(invocation -> {

			String indexName = (String) invocation.getArguments()[0];
			String indexedExpression = (String) invocation.getArguments()[1];
			String fromClause = (String) invocation.getArguments()[2];

			return mockIndex(indexName, org.apache.geode.cache.query.IndexType.FUNCTIONAL, indexedExpression,
				fromClause, null);
		});

		when(queryService.createIndex(anyString(), anyString(),anyString(),anyString())).thenAnswer(invocation -> {

			String indexName = (String) invocation.getArguments()[0];
			String indexedExpression = (String) invocation.getArguments()[1];
			String fromClause = (String) invocation.getArguments()[2];
			String imports = (String) invocation.getArguments()[3];

			return mockIndex(indexName, org.apache.geode.cache.query.IndexType.FUNCTIONAL, indexedExpression,
				fromClause, imports);
		});

		when(queryService.createKeyIndex(anyString(), anyString(),anyString())).thenAnswer(invocation -> {

			String indexName = (String) invocation.getArguments()[0];
			String indexedExpression = (String) invocation.getArguments()[1];
			String fromClause = (String) invocation.getArguments()[2];

			return mockIndex(indexName, org.apache.geode.cache.query.IndexType.PRIMARY_KEY, indexedExpression,
				fromClause, null);
		});

		when(queryService.createHashIndex(anyString(), anyString(),anyString())).thenAnswer(invocation -> {

			String indexName = (String) invocation.getArguments()[0];
			String indexedExpression = (String) invocation.getArguments()[1];
			String fromClause = (String) invocation.getArguments()[2];

			return mockIndex(indexName, org.apache.geode.cache.query.IndexType.HASH, indexedExpression,
				fromClause, null);
		});

		when(queryService.createHashIndex(anyString(), anyString(),anyString(),anyString())).thenAnswer(invocation -> {

			String indexName = (String) invocation.getArguments()[0];
			String indexedExpression = (String) invocation.getArguments()[1];
			String fromClause = (String) invocation.getArguments()[2];
			String imports = (String) invocation.getArguments()[3];

			return mockIndex(indexName, org.apache.geode.cache.query.IndexType.HASH, indexedExpression,
				fromClause, imports);
		});

		return queryService;
	}

	@SuppressWarnings({ "rawtypes", "unchecked", "unused" })
	Index mockIndex(String indexName, org.apache.geode.cache.query.IndexType indexType, String indexedExpression,
		String fromClause, String imports){
		Index idx = mock(Index.class);
		when(idx.getFromClause()).thenReturn(fromClause);
		when(idx.getIndexedExpression()).thenReturn(indexedExpression);
		when(idx.getName()).thenReturn(indexName);
		when(idx.getType()).thenReturn(indexType);


		if (fromClause != null && fromClause.length() >= 2) {
			Region region = mock(Region.class);
			String name = fromClause.substring(1).split(" ")[0];
			when(region.getName()).thenReturn(name);
			when(idx.getRegion()).thenReturn(region);
		}
		return idx;
	}

	@SuppressWarnings("rawtypes")
	public Map<String,Region> allRegions() {
		return this.rootRegions;
	}

	public void setProperties(Properties gemfireProperties) {
		this.properties = gemfireProperties;
	}

	@Override
	public boolean isReconnecting() {
		return false;
	}

	@Override
	public Cache getReconnectedCache() {
		return this;
	}

	@Override
	public void stopReconnecting() {
	}

	@Override
	public boolean waitUntilReconnected(final long l, final TimeUnit timeUnit) throws InterruptedException {
		return false;
	}

	public RegionService createAuthenticatedView(final Properties userSecurityProperties) {
		return this;
	}

	@Override
	public RegionService createAuthenticatedView(final Properties userSecurityProperties, final String poolName) {
		return this;
	}

	@Override
	public <K, V> ClientRegionFactory<K, V> createClientRegionFactory(ClientRegionShortcut shortcut) {
		return new MockClientRegionFactory<K, V>(this).createClientRegionFactory(shortcut);
	}

	@Override
	public <K, V> ClientRegionFactory<K, V> createClientRegionFactory(String regionAttributesId) {
		return new MockClientRegionFactory<K, V>(this).createClientRegionFactory();
	}

	@Override
	public Set<InetSocketAddress> getCurrentServers() {
		return Collections.emptySet();
	}

	@Override
	public Pool getDefaultPool() {
		return null;
	}

	@Override
	public QueryService getLocalQueryService() {
		return getQueryService();
	}

	@Override
	public QueryService getQueryService(final String poolName) {
		return getQueryService();
	}

	@Override
	public void registerPdxMetaData(Object objectToSerializeAndRegister) { }

}
