/*
 * Copyright 2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.transaction.event;

import java.util.Properties;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.TransactionEvent;
import org.apache.geode.cache.TransactionWriter;
import org.apache.geode.cache.TransactionWriterException;

import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.util.Assert;

/**
 * An implementation of Apache Geode's {@link TransactionWriter} interface that uses the {@literal Composite Software Design}
 * Pattern to compose multiple {@link TransactionWriter} objects into a single instance.
 *
 * @author John Blum
 * @see org.apache.geode.cache.TransactionWriter
 * @since 2.3.0
 */
public class ComposableTransactionWriter implements TransactionWriter {

	/**
	 * Factory method used to construct and compose 2 {@link TransactionWriter} objects into a composite instance of
	 * {@link TransactionWriter} functioning as a single instance.
	 *
	 * @param transactionWriterOne first {@link TransactionWriter} in the composition.
	 * @param transactionWriterTwo second {@link TransactionWriter} in the composition.
	 * @return the first {@link TransactionWriter} if the second {@link TransactionWriter} is {@literal null}, or return
	 * the second {@link TransactionWriter} if the first {@link TransactionWriter} is {@literal null}, or return
	 * the composition of both {@link TransactionWriter} one and {@link TransactionWriter} two.
	 * @see org.apache.geode.cache.TransactionWriter
	 */
	public static @Nullable TransactionWriter compose(@Nullable TransactionWriter transactionWriterOne,
			@Nullable TransactionWriter transactionWriterTwo) {

		return transactionWriterOne == null ? transactionWriterTwo
			: transactionWriterTwo == null ? transactionWriterOne
			: new ComposableTransactionWriter(transactionWriterOne, transactionWriterTwo);
	}

	private final TransactionWriter transactionWriterOne;
	private final TransactionWriter transactionWriterTwo;

	private ComposableTransactionWriter(@NonNull TransactionWriter transactionWriterOne,
			@NonNull TransactionWriter transactionWriterTwo) {

		Assert.notNull(transactionWriterOne, "TransactionWriter one must not be null");
		Assert.notNull(transactionWriterTwo, "TransactionWriter two must not be null");

		this.transactionWriterOne = transactionWriterOne;
		this.transactionWriterTwo = transactionWriterTwo;
	}

	/**
	 * Returns a reference to the first {@link TransactionWriter} in the composition.
	 *
	 * @return a reference to the first {@link TransactionWriter} in the composition.
	 * @see org.apache.geode.cache.TransactionWriter
	 */
	protected TransactionWriter getTransactionWriterOne() {
		return this.transactionWriterOne;
	}

	/**
	 * Returns a reference to the second {@link TransactionWriter} in the composition.
	 *
	 * @return a reference to the second {@link TransactionWriter} in the composition.
	 * @see org.apache.geode.cache.TransactionWriter
	 */
	protected TransactionWriter getTransactionWriterTwo() {
		return this.transactionWriterTwo;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void beforeCommit(TransactionEvent event) throws TransactionWriterException {

		getTransactionWriterOne().beforeCommit(event);
		getTransactionWriterTwo().beforeCommit(event);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void close() {

		getTransactionWriterOne().close();
		getTransactionWriterTwo().close();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void init(Properties properties) {

		getTransactionWriterOne().init(properties);
		getTransactionWriterTwo().init(properties);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void initialize(Cache cache, Properties properties) {

		getTransactionWriterOne().initialize(cache, properties);
		getTransactionWriterTwo().initialize(cache, properties);
	}
}
